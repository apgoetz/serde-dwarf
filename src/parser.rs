use crate::{DebugInfo, DebugInfoBuilder, Error, ErrorCode, ErrorImpl, Filter, Result};
use crate::typ;
use gimli::{self, constants};
use object::{self, Object, ObjectSection};
use rustc_demangle::demangle;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Bound;
use std::result;
use std::sync;
use std::convert::TryFrom;
use fallible_iterator::FallibleIterator;
use rayon::prelude::*;

type Reader<'i> = gimli::EndianSlice<'i, gimli::RunTimeEndian>;
type UnitOffset<'i> = gimli::UnitOffset<<Reader<'i> as gimli::Reader>::Offset>;
type AttributeValue<'i> = gimli::AttributeValue<Reader<'i>>;

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
            if let Ok(_) = unitparser.parse() {
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
    fn dump_die(
        &self,
        depth: usize,
        entry: &gimli::DebuggingInformationEntry<Reader>,
    ) -> result::Result<(), gimli::Error> {
        let padding = "  ".repeat(2 * depth + 1 as usize);
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
    fn get_str_val(
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
struct UnitParser<'i> {
    // reference the unit we are iterating over
    unit: &'i gimli::Unit<Reader<'i>>,
    // map between symbols as strings (user interface) and the type (as unit offset)
    symbol_types: HashMap<&'i str, gimli::DebugInfoOffset>,
    // info on types we want to support
    serialize_types: HashSet<gimli::DebugInfoOffset>,
    // the current namesapce we are in,
    ns_parts: Vec<(&'i str, usize)>,
    // range of offsets that map to namespaces
    ns_ranges: BTreeMap<UnitOffset<'i>, Option<String>>,
    di_parser: &'i DebugInfoParser<'i>,
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

// contains the results of all of our unit parsing
struct UnitResults<'i> {
    // mapping of parser results to the sections of the file they came from
    namespaces: BTreeMap<gimli::DebugInfoOffset, Option<String>>,
    units: BTreeMap<gimli::DebugInfoOffset, &'i gimli::Unit<Reader<'i>>>,
    // reference to the root parser (contains the options and dwarf object
    di_parser: &'i DebugInfoParser<'i>,
    // symbols we want to export
    symbol_types: HashMap<&'i str, gimli::DebugInfoOffset>,
    // info on serializable types we want to support
    serialize_types: HashSet<gimli::DebugInfoOffset>,
}

impl<'i> UnitResults<'i> {
    fn new(di_parser: &'i DebugInfoParser<'i>) -> Self {
        UnitResults {
            di_parser,
            namespaces: BTreeMap::new(),
            units: BTreeMap::new(),
            symbol_types: HashMap::new(),
            serialize_types: HashSet::new(),
        }
    }

    fn add_unit(&mut self, unit: UnitParser<'i>) {
        let UnitParser {
            symbol_types,
            serialize_types,
            ns_ranges,
            unit,
            ..
        } = unit;

        if let gimli::UnitSectionOffset::DebugInfoOffset(offset) = unit.header.offset() {
            // add all of the valid namespaces
            self.namespaces.extend(
                ns_ranges
                    .into_iter()
                    .filter_map(|(k, v)| unit_to_di(unit, k).map(|k| (k, v))),
            );

            self.symbol_types.extend(symbol_types.into_iter());

            self.serialize_types.extend(serialize_types.into_iter());

            self.units.insert(offset, unit);
        }
    }

    // this is a big function, it parses a  DIE  and maps it onto
    // the Serde data model
    fn add_type(&self,
                builder: &mut typ::TypeBuilder<gimli::DebugInfoOffset>,
                offset: gimli::DebugInfoOffset) -> Option<()> {
        // if we have visited this type before, we can leave, it is already added 
        if builder.contains_key(offset) {return Some(());}
        // get the local offset
        let (unit, u_offset) = self.di_to_unit(offset)?;
        // if we can build an entry here
        let entry = unit.entry(u_offset).ok()?;
        let name = self.di_parser.get_str_val(&entry, constants::DW_AT_name);
        let _namespace = self.get_namespace(offset);
        match entry.tag() {
            constants::DW_TAG_base_type => {
                let prim = typ::PrimitiveType::try_from(name.ok()??).ok()?;
                builder.add_prim(offset, prim);
            }
            _ => {
                // eprintln!("Cannot add type:");
                // self.di_parser.dump_die(0, &entry).ok()?;
                return None;
            },
        }
        Some(())
    }

    // constructs a typebuilder and adds  a type
    fn get_typebuilder(&self,
                       offset: gimli::DebugInfoOffset)
                       -> Option<typ::TypeBuilder<gimli::DebugInfoOffset>> {
        let mut builder = typ::TypeBuilder::new();
        self.add_type(&mut builder, offset)?;
        Some(builder)
    }
    
    // converts a DIE that represents a function into the self pointer that represents it
    fn subfunction_to_self(
        &self,
        offset: gimli::DebugInfoOffset,
    ) -> Option<gimli::DebugInfoOffset> {
        // find the local unit this subfunction was defined in
        if let Some((unit, offset)) = self.di_to_unit(offset) {
            let mut tree = unit.entries_tree(Some(offset)).ok()?;
            let mut parameters = tree.root().ok()?.children();
            while let Some(node) = parameters.next().ok()? {
                let entry = node.entry();
                if entry.tag() == constants::DW_TAG_formal_parameter {
                    if let Some(name) = self
                        .di_parser
                        .get_str_val(entry, constants::DW_AT_name)
                        .ok()?
                    {
                        if name == "self" {
                            // we have found the type of self, so return it
                            let ptr = get_type(unit, entry).ok()?;
                            // deref this pointer to get to the underlying type
                            return self.deref_ptr(ptr).ok();
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
            }
        }
        // if we got here, we failed
        None
    }

    // converter a debuginfo offset into the unit it came from and the redirected unit offset
    fn di_to_unit(
        &self,
        offset: gimli::DebugInfoOffset,
    ) -> Option<(&gimli::Unit<Reader>, UnitOffset<'i>)> {
        // if we can find a unit that corresponds to this offset
        if let Some(unit) = self
            .units
            .range((Bound::Unbounded, Bound::Included(&offset)))
            .next_back()
            .map(|u| u.1)
        {
            // we found a unit, now lets convert the info offset
            // into a local offset. This should always succeed.
            if let Some(offset) = offset.to_unit_offset(&unit.header) {
                return Some((unit, offset));
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_namespace(&self, offset: gimli::DebugInfoOffset) -> Option<Option<&str>> {
        self.namespaces
            .range((Bound::Unbounded, Bound::Included(&offset)))
            .next_back()
            .map(|n| n.1)
            .map(Option::as_deref)
    }

    fn get_typename(&self, offset: gimli::DebugInfoOffset) -> Option<String> {
        if let Some((name, namespace)) = self.get_fullname(offset) {
            if namespace.len() > 0 {
                Some(format!("{}::{}", namespace, name))
            } else {
                Some(name)
            }
        } else { None }
    }
    
    // get the typename of a type. if the type is invalid, or is
    // located in a hidden namespace, the None is returned
    // returns (name, namespace)
    fn get_fullname(&self, offset: gimli::DebugInfoOffset) -> Option<(String, String)> {
        // first, get the unit and offset this type came from
        if let Some((unit, u_offset)) = self.di_to_unit(offset) {
            // need to get the DIE that this entry coresponds to
            if let Ok(entry) = unit.entry(u_offset) {
                let name = self.di_parser.get_str_val(&entry, constants::DW_AT_name);
                let namespace = self.get_namespace(offset);
                match entry.tag() {
                    constants::DW_TAG_structure_type => {
                        if let Ok(Some(name)) = name {
                            match namespace {
                                // if we found a namespace, and it is valud
                                Some(Some(namespace)) => {
                                    Some((name.to_string(), String::from(namespace)))
                                }
                                // we found a namespace, but it is blacklisted
                                Some(None) => None,
                                // we did not a namespace for this symbol
                                None => Some((String::from(name), "".to_string())),
                            }
                        } else {
                            None
                        }
                    }
                    constants::DW_TAG_base_type => {
                        if let Ok(name) = name {
                            name.map(|n| (String::from(n), "".to_string()))
                        } else {
                            None
                        }
                    }
                    constants::DW_TAG_array_type => {
                        let subtype = get_type(unit, &entry).ok()?;

                        let (name,namespace) = self.get_fullname(subtype)?;
                        let len = get_array_length(unit, u_offset)?;

                        if namespace.len() > 0 {
                            Some((format!("[{}::{}; {}]", namespace, name, len), String::new()))
                        } else {
                            Some((format!("[{}; {}]", name, len), String::new()))
                        }
                    },
                    _ => {
                        if let Ok(Some(name)) = name {
                            println!("unknown type: {}", name);
                        }
                        None
                    }
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    //takes an offset to a pointer type, and returns the type that pointer points at
    fn deref_ptr(
        &self,
        offset: gimli::DebugInfoOffset,
    ) -> result::Result<gimli::DebugInfoOffset, gimli::Error> {
        if let Some((unit, offset)) = self.di_to_unit(offset) {
            let entry = unit.entry(offset)?;
            match entry.tag() {
                // it is another pointer, we need to dereference it
                constants::DW_TAG_pointer_type => {
                    let ptr = get_type(unit, &entry)?;
                    return self.deref_ptr(ptr);
                }
                // it is a concrete type, we need to return the offset of this type
                constants::DW_TAG_structure_type
                | constants::DW_TAG_base_type
                | constants::DW_TAG_array_type => {
                    if let Some(di) = unit_to_di(unit, entry.offset()) {
                        return Ok(di);
                    } else {
                        return Err(gimli::Error::InvalidLocationAddressRange);
                    }
                }
                _ => {}
            };
            Err(gimli::Error::UnsupportedAttributeForm)
        } else {
            // we could not determine the unit for this address
            Err(gimli::Error::InvalidLocationAddressRange)
        }
    }
}

// convert a unit offset into a debug info offset
fn unit_to_di(unit: &gimli::Unit<Reader>, offset: UnitOffset) -> Option<gimli::DebugInfoOffset> {
    offset.to_debug_info_offset(&unit.header)
}

// read out a a DIE attribute as reference
//
// currently only implemented for unit offsets, which means fat LTO breaks performance
fn get_type(
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
fn get_array_length(unit: &gimli::Unit<Reader>, offset: UnitOffset) -> Option<u64>{
    // build a cursor pointing at the array type
    let mut cursor = unit.entries_at_offset(offset).ok()?;
    // move cursor to this element
    if cursor.next_dfs().ok()?.is_none() {
        return None;
    }
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
