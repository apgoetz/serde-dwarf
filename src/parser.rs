use crate::{DebugInfo, DebugInfoBuilder, Error, ErrorCode, ErrorImpl, Filter, Result};
use crate::typ;
use crate::entry_parser::UnitResults;
use gimli::{self, constants};
use object::{self, Object, ObjectSection};
use rustc_demangle::demangle;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::result;
use std::sync;
use fallible_iterator::FallibleIterator;
use rayon::prelude::*;



pub type Reader<'i> = gimli::EndianSlice<'i, gimli::RunTimeEndian>;
pub type UnitOffset<'i> = gimli::UnitOffset<<Reader<'i> as gimli::Reader>::Offset>;
pub type AttributeValue<'i> = gimli::AttributeValue<Reader<'i>>;

pub struct DebugInfoParser<'i> {
    object: &'i object::File<'i>,
    dwarf: gimli::Dwarf<Reader<'i>>,
    options: &'i DebugInfoBuilder,
}

impl<'i> DebugInfoParser<'i> {
    /// parse the data in an existing object
    pub fn new(options: &'i DebugInfoBuilder, object: &'i object::File<'i>) -> Result<Self> {
        // most of the code in this function is based on the simple.rs example in the gimli crate
        let endian = if object.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        let load_section = |id: gimli::SectionId| -> result::Result<
            gimli::EndianSlice<'i, gimli::RunTimeEndian>,
            gimli::Error,
        > {
            let empty = Ok(gimli::EndianSlice::new(&[], endian));
            match object.section_by_name(id.name()) {
                Some(ref section) => {
                    if let Ok(data) = section.compressed_data() {
                        if data.format == object::CompressionFormat::None {
                            return Ok(gimli::EndianSlice::new(&data.data, endian));
                        }
                    }
                    empty
                }
                None => empty,
            }
        };
        // Load a supplementary section. We don't have a supplementary object file,
        // so always return an empty slice.
        let load_section_sup = |_| Ok(gimli::EndianSlice::new(&[], endian));

        // Load all of the sections.
        let dwarf = gimli::Dwarf::load(&load_section, &load_section_sup);
        let dwarf = dwarf.map_err(|e| {
            Error(ErrorImpl {
                code: ErrorCode::Gimli(e),
            })
        })?;

        Ok(DebugInfoParser {
            object,
            dwarf,
            options,
        })
    }

    // do all the work to make the debuginfo parser
    pub fn parse(&self) -> Result<DebugInfo> {

        // vector of all units to work on
        let units = self
            .dwarf
            .units()
            .filter(|h| Ok(h.offset().as_debug_info_offset().is_some()))
            .map(|h| self.dwarf.unit(h))
            .collect::<Vec<_>>()?;

        let results = sync::Mutex::new(UnitResults::new(self));

        let process_unit = |unit: &'i gimli::Unit<Reader<'i>>| {
            let mut unitparser = UnitParser::new(self, unit);
            if unitparser.parse().is_ok() {
                results.lock().unwrap().add_unit(unitparser);
            }
        };
        units.par_iter().for_each(process_unit);

        // now the unitresults object has all of the  types to print, we can throw away the mutex
        let mut results = results.into_inner().unwrap();

        // update serialize_types by replacing them with the types of
        // their "self" parameter
        results.serialize_types = results
            .serialize_types
            .par_iter()
            .filter_map(|o| results.subfunction_to_self(*o))
            .collect::<HashSet<_>>();


        
        let mut builder = typ::TypeBuilder::new();
        results
            .serialize_types
            .iter()
            .for_each(|o| results.add_type(&mut builder, *o).unwrap_or(()));

        results
            .symbol_types
            .values()
            .for_each(|o| results.add_type(&mut builder, *o).unwrap_or(()));

        let types = builder
            .iter()
            .filter_map(|o| results.get_typename(*o).map(|n|(n, *o)))
            .collect::<HashMap<String,gimli::DebugInfoOffset>>();


        let syms = results
            .symbol_types
            .iter()
            .map(|(k,v)| (String::from(*k), *v))
            .collect::<HashMap<String,gimli::DebugInfoOffset>>();

        let symtypes = results
            .symbol_types
            .iter()
            .filter_map(|(k,v)| results.get_typename(*v).map(|n|(String::from(*k), n)))
            .collect::<HashMap<String, String>>();

        
        Ok(DebugInfo {types, syms, symtypes, builder})
    }

    // helper debug functions to print info for a die
    pub fn dump_die(
        &self,
        depth: usize,
        entry: &gimli::DebuggingInformationEntry<Reader>,
    ) -> result::Result<(), gimli::Error> {
        let padding = "  ".repeat(2 * depth + 1 );
        let tag_padding = "  ".repeat(2 * depth as usize);
        eprintln!(
            "{}<{}>{}",
            tag_padding,
            depth,
            entry.tag().static_string().unwrap()
        );

        let mut attrs = entry.attrs();

        while let Some(attr) = attrs.next()? {
            eprint!("{}", padding);

            if let Some(n) = attr.name().static_string() {
                eprint!("{}: ", n);
            } else {
                eprint!("{:27}: ", attr.name());
            }

            if let gimli::AttributeValue::DebugStrRef(offset) = attr.value() {
                let string = self.dwarf.debug_str.get_str(offset)?;
                let string = demangle(string.to_string()?).to_string();
                eprintln!("{}", string);
            } else if let Some(num) = attr.udata_value() {
                eprintln!("{}", num);
            } else if let gimli::AttributeValue::UnitRef(offset) = attr.value() {
                eprintln!("unit+{:#010x}", offset.0);
            } else {
                eprintln!("??? {:#?}", attr.value());
            }
        }
        Ok(())
    }
    
    pub fn get_str_val(
        &self,
        entry: &gimli::DebuggingInformationEntry<Reader>,
        at: constants::DwAt,
    ) -> result::Result<Option<&'i str>, gimli::Error> {
        if let Some(attr) = entry.attr(at)? {
            if let gimli::AttributeValue::DebugStrRef(offset) = attr.value() {
                Ok(Some(self.dwarf.debug_str.get_str(offset)?.to_string()?))
            } else {
                Err(gimli::Error::TypeMismatch)
            }
        } else {
            Ok(None)
        }
    }
    // checks if a given symbol should be added to the debuginfo results
    // if there is a symbol filter, we add that symbol
    // if there is no symbol filter, we do not collect symbols
    fn allowed_by_sym_filter(&self, name: &'i str) -> bool {
        let filter = &self.options.filter;
        match filter {
            Filter::SymbolRegex(r) => r.is_match(name),
            Filter::SymbolList(set) => set.get(name).is_some(),
            _ => false,
        }
    }

}

#[derive(Debug)]
enum TypeName<'i> {
    String(&'i str),
    Array {
        size: u64,
        element: AttributeValue<'i>,
    },
}

// parse all the info we need from a single unt
pub struct UnitParser<'i> {
    // reference the unit we are iterating over
    pub unit: &'i gimli::Unit<Reader<'i>>,
    // map between symbols as strings (user interface) and the type (as unit offset)
    pub symbol_types: HashMap<&'i str, gimli::DebugInfoOffset>,
    // info on types we want to support
    pub serialize_types: HashSet<gimli::DebugInfoOffset>,
    // the current namesapce we are in,
    pub ns_parts: Vec<(&'i str, usize)>,
    // range of offsets that map to namespaces
    pub ns_ranges: BTreeMap<UnitOffset<'i>, Option<String>>,
    pub di_parser: &'i DebugInfoParser<'i>,
}

impl<'i> UnitParser<'i> {
    pub fn new(di_parser: &'i DebugInfoParser<'i>, unit: &'i gimli::Unit<Reader<'i>>) -> Self {
        UnitParser {
            di_parser,
            unit,
            ns_ranges: BTreeMap::new(),
            ns_parts: Vec::new(),
            symbol_types: HashMap::new(),
            serialize_types: HashSet::new(),
        }
    }

    pub fn parse(&mut self) -> result::Result<(), gimli::Error> {
        let unit = &self.unit;
        let mut entries = unit.entries();
        let mut depth = 0;

        // loop over all DIEs in this unit
        while let Some((delta_depth, entry)) = entries.next_dfs()? {
            // cleanup the count of our current depth
            depth += delta_depth;
            assert!(depth >= 0);

            // cleanup the current namespace if we just left one
            self.remove_old_namespace(depth as usize, entry.offset());

            self.first_pass(entry, depth as usize)?;
        }
        Ok(())
    }

    // first pass through all of the DIEs in the unit.
    fn first_pass(
        &mut self,
        entry: &gimli::DebuggingInformationEntry<Reader>,
        depth: usize,
    ) -> result::Result<(), gimli::Error> {
        match entry.tag() {
            constants::DW_TAG_variable => {
                if let Some(name) = self.di_parser.get_str_val(entry, constants::DW_AT_name)? {
                    // if the symbol we see doesnt match our
                    // filter, we dont need to do any more work
                    if !self.di_parser.allowed_by_sym_filter(name) {
                        return Ok(());
                    }

                    let offset = get_type(&self.unit, entry)?;
                    self.symbol_types.insert(name, offset);
                }
            }
            constants::DW_TAG_namespace => {
                if let Some(namespace) = self.di_parser.get_str_val(entry, constants::DW_AT_name)? {
                    // add the new namespace part we encountered
                    self.ns_parts.push((namespace, depth));
                    // update the latest namespace Id
                    self.render_namespace(entry.offset());
                } else {
                    return Err(gimli::Error::UnsupportedAttributeForm);
                }
            }
            constants::DW_TAG_subprogram => {
                if let Some(export) = self
                    .di_parser
                    .get_str_val(entry, constants::DW_AT_linkage_name)?
                {
                    let export = demangle(export).to_string();

                    // use heuristics to guess if a function
                    // implements a seriaizer this currently works for
                    // legacy demangling algo, need to confirm it
                    // works for v0 as well
                    if export.contains("impl serde::ser::Serialize for") {
                        // convert the unit offset of this DIE to a debuginfo offset
                        if let Some(di_offset) = unit_to_di(&self.unit, entry.offset()) {
                            self.serialize_types.insert(di_offset);
                        }
                    }
                }
            }
            _ => { /* ignore the rest of the tag types */ }
        }
        Ok(())
    }

    // cleanup namespace stack if we encounter a new namespace
    fn remove_old_namespace(&mut self, curdepth: usize, cur_offset: UnitOffset) {
        let oldlen = self.ns_parts.len();
        while let Some((_, last_depth)) = self.ns_parts.last() {
            if *last_depth >= curdepth {
                self.ns_parts.pop();
            } else {
                break;
            }
        }
        // if we updated the namespace
        if oldlen != self.ns_parts.len() {
            self.render_namespace(cur_offset)
        }
    }

    fn render_namespace(&mut self, cur_offset: UnitOffset<'i>) {
        let rendered = self
            .ns_parts
            .iter()
            .map(|t| t.0)
            .collect::<Vec<&str>>()
            .join("::");

        let ns = if self.di_parser.options.allow_namespace(&rendered) {
            Some(rendered)
        } else {
            None
        };
        self.ns_ranges.insert(cur_offset, ns);
    }
}

// convert a unit offset into a debug info offset
pub fn unit_to_di(unit: &gimli::Unit<Reader>, offset: UnitOffset) -> Option<gimli::DebugInfoOffset> {
    offset.to_debug_info_offset(&unit.header)
}

// read out a a DIE attribute as reference
//
// currently only implemented for unit offsets, which means fat LTO breaks performance
pub fn get_type(
    unit: &gimli::Unit<Reader>,
    entry: &gimli::DebuggingInformationEntry<Reader>,
) -> result::Result<gimli::DebugInfoOffset, gimli::Error> {
    if let Some(attr) = entry.attr(constants::DW_AT_type)? {
        match attr.value() {
            gimli::AttributeValue::UnitRef(offset) => {
                if let Some(di_offset) = unit_to_di(unit, offset) {
                    Ok(di_offset)
                } else {
                    Err(gimli::Error::TypeMismatch)
                }
            }
            gimli::AttributeValue::DebugInfoRef(offset) => Ok(offset),
            _ => Err(gimli::Error::TypeMismatch),
        }
    } else {
        Err(gimli::Error::UnsupportedAttributeForm)
    }
}

// get the length of an array
pub fn get_array_length(unit: &gimli::Unit<Reader>, offset: UnitOffset) -> Option<u64>{
    // build a cursor pointing at the array type
    let mut cursor = unit.entries_at_offset(offset).ok()?;
    // move cursor to this element
    cursor.next_dfs().ok()??;

    // move cursor to child, describing the array
    if let Some((delta_depth, entry)) = cursor.next_dfs().ok()? {
        // if we did not move directly to our own child:
        if delta_depth != 1 {
            None
            //Err(Error::ParseError("depth of next element not equal to 1".to_string()))
        } else {
            let has_attr = entry.attr(constants::DW_AT_count).ok()?;
            if let Some(attr) = has_attr {
                if let Some(length) = attr.value().udata_value() {
                    Some(length)
                } else {
                    Some(0)
                }
            } else {
                Some(0)
            }
        }
    } else {
        None
        //Err(Error::ParseError("Could not travere to another node after array".to_string()))
    }
}
