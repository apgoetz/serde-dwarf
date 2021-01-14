use crate::{DebugInfo, DebugInfoBuilder, Error, ErrorCode, ErrorImpl, Filter, Result};
use fallible_iterator::FallibleIterator;
use gimli::{self, constants};
use object::{self, Object, ObjectSection};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::result;
use std::ops::Bound;
use rustc_demangle::demangle;
// trait Reader: gimli::Reader<Offset = usize> + Send + Sync {}
// impl<'input, Endian> Reader for gimli::EndianSlice<'input, Endian> where
//     Endian: gimli::Endianity + Send + Sync
// {
// }
type Reader<'i> = gimli::EndianSlice<'i, gimli::RunTimeEndian>;
type UnitOffset<'i> = gimli::UnitOffset<<Reader<'i> as gimli::Reader>::Offset>;

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

    pub fn parse(&self) -> Result<DebugInfo> {
        let types = HashMap::new();
        let syms = HashMap::new();

        // try to get the units from the dwarf file, or print error
        let _result = self
            .dwarf
            .units()
            .map(|h| self.dwarf.unit(h))
            .map(|u| UnitParser::new(self, &u).parse())
            .collect::<Vec<()>>()?;

        Ok(DebugInfo { types, syms })
    }
}

#[derive(Debug)]
enum TypeName<'i> {
    String(&'i str),
    Array { size: u64, element: UnitOffset<'i> },
}


struct UnitParser<'i> {
    // point to parent parser
    parser: &'i DebugInfoParser<'i>,
    // reference the unit we are iterating over
    unit: &'i gimli::Unit<Reader<'i>>,
    // map between symbols as strings (user interface) and the type (as unit offset)
    symbol_map: HashMap<&'i str, UnitOffset<'i>>,
    // info on types we want to support
    types: HashMap<&'i str, UnitOffset<'i>>,
    // functions we think might be serializable
    serialize_funcs: HashSet<UnitOffset<'i>>,
    // the current namesapce we are in,
    ns_parts: Vec<(&'i str, usize)>,
    // range of offsets that map to namespaces
    ns_ranges: BTreeMap<UnitOffset<'i>, Option<String>>,
}

impl<'i> UnitParser<'i> {
    pub fn new(parser: &'i DebugInfoParser, unit: &'i gimli::Unit<Reader<'i>>) -> Self {
        UnitParser {
            parser,
            unit,
            ns_ranges: BTreeMap::new(),
            ns_parts: Vec::new(),
            symbol_map: HashMap::new(),
            types: HashMap::new(),
            serialize_funcs: HashSet::new(),
        }
    }

    pub fn parse(&mut self) -> result::Result<(), gimli::Error> {
        let mut entries = self.unit.entries();
        let mut depth = 0;

        // loop over all DIEs in this unit
        while let Some((delta_depth, entry)) = entries.next_dfs()? {
            // cleanup the count of our current depth
            depth += delta_depth;
            assert!(depth >= 0);

            // cleanup the current namespace if we just left one
            self.remove_old_namespace(depth as usize, entry.offset());

            if let Err(e) = self.first_pass(entry, depth as usize) {
                // eprintln!("Could not parse unit!");
                // self.dump_die(0, entry).unwrap();
                return Err(e);
            }

            // at this point, we know the namespaces and the
            // serialize functions, and the variables that may be
            // exported. Now we do a second pass to extract the needed types
            let mut ser_types = HashSet::new();
            
            for func_offset in self.serialize_funcs.iter() {
		let mut tree = self.unit.entries_tree(Some(*func_offset))?;
		let mut parameters = tree.root()?.children();
		while let Some(node) = parameters.next()? {
		    let entry = node.entry();
		    if entry.tag() == constants::DW_TAG_formal_parameter {
			if let Some(name) = self.get_str_val(entry, constants::DW_AT_name)? {
			    if name == "self" {
				let ptr_offset = self.get_unit_offset(entry, constants::DW_AT_type)?;
				ser_types.insert(self.deref_ptr(ptr_offset)?);
			    }
			}
		    }
		}
            }


	    for offset in ser_types.iter() {
                if let Some(name) = self.get_typename(*offset){
                    println!("{}", name);
                } else {
                    eprintln!("Unknown type:");
                    self.dump_die(0, &self.unit.entry(*offset)?)?;
                }
		
	    }
		

        }
        Ok(())
    }

    // takes an offset to a pointer type, and returns the type that pointer points at
    fn deref_ptr(&self, offset: UnitOffset<'i>) -> result::Result<UnitOffset<'i>, gimli::Error> {
	let entry = self.unit.entry(offset)?;
	match entry.tag()  {
            constants::DW_TAG_pointer_type => {
	        let ptr =  self.get_unit_offset(&entry, constants::DW_AT_type)?;
                return self.deref_ptr(ptr);
            }
            constants::DW_TAG_structure_type
                | constants::DW_TAG_base_type
                | constants::DW_TAG_array_type => {
                    return Ok(entry.offset());
            },
            _ => {},
	};
	Err(gimli::Error::UnsupportedAttributeForm)
    }

    // first pass through all of the DIEs in the unit. 
    fn first_pass(
        &mut self,
        entry: &gimli::DebuggingInformationEntry<Reader>,
        depth: usize,
    ) -> result::Result<(), gimli::Error> {
        match entry.tag() {
            constants::DW_TAG_variable => {
                if let Some(name) = self.get_str_val(entry, constants::DW_AT_name)? {
                    // if the symbol we see doesnt match our
                    // filter, we dont need to do any more work
                    if !self.allowed_by_sym_filter(name) {
                        return Ok(());
                    }

                    let offset = self.get_unit_offset(entry, constants::DW_AT_type)?;
                    self.symbol_map.insert(name, offset);
                }
            }
            constants::DW_TAG_namespace => {
                if let Some(namespace) = self.get_str_val(entry, constants::DW_AT_name)? {
                    // add the new namespace part we encountered
                    self.ns_parts.push((namespace, depth));
                    // update the latest namespace Id
                    self.render_namespace(entry.offset());
                } else {
                    return Err(gimli::Error::UnsupportedAttributeForm);
                }
            }
            constants::DW_TAG_subprogram => {
                if let Some(export) = self.get_str_val(entry, constants::DW_AT_linkage_name)? {
                    let export = demangle(export).to_string();
                        
                    // use heuristics to guess if a function
		    // implements a seriaizer this currently works for
		    // legacy demangling algo, need to confirm it
		    // works for v0 as well
                    if export.contains("impl serde::ser::Serialize for")
                    {
                        self.serialize_funcs.insert(entry.offset());
                    }
                    
                }
            }
            _ => { /* ignore the rest of the tag types */ }
        }
        Ok(())
    }

    // cleanup namespace stack if we encounter a new namespace
    fn remove_old_namespace(&mut self, curdepth: usize, cur_offset: UnitOffset<'i>) {
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
        let denylist = ["{{impl}}","{{closure}}"];
        let rendered = self
            .ns_parts
            .iter()
            .map(|t| t.0)
            .collect::<Vec<&str>>()
            .join("::");

        // check to see if this namespace contains a forbidden namespace
        for deny in denylist.iter() {
            if rendered.contains(deny) {
                self.ns_ranges.insert(cur_offset, None);
                return;
            }
        }
        
        self.ns_ranges.insert(cur_offset, Some(rendered));
    }

    // read out a a DIE attribute as reference
    //
    // currently only implemented for unit offsets, which means fat LTO breaks performance
    fn get_unit_offset(
        &self,
        entry: &gimli::DebuggingInformationEntry<Reader>,
        at: constants::DwAt,
    ) -> result::Result<UnitOffset, gimli::Error> {
        if let Some(attr) = entry.attr(at)? {
            match attr.value() {
                gimli::AttributeValue::UnitRef(offset) => {
                Ok(offset)
                },
                gimli::AttributeValue::DebugInfoRef(offset) => {
                    // fat lto currently breaks serde_dwarf, because
                    // type references start to refer to other
                    // compilation units, so we cannot complete all parsing in parallel
                    if let Some(unitoffset) =  offset.to_unit_offset(&self.unit.header) {
                        Ok(unitoffset)
                    } else {
                        self.dump_die(0, entry)?;
                        Err(gimli::Error::OffsetOutOfBounds)
                    }
                },
                _ => {
                    self.dump_die(0, entry)?;
                    Err(gimli::Error::TypeMismatch)
                }
            }
        } else {
            Err(gimli::Error::UnsupportedAttributeForm)
        }
    }

    fn get_str_val(
        &self,
        entry: &gimli::DebuggingInformationEntry<Reader>,
        at: constants::DwAt,
    ) -> result::Result<Option<&'i str>, gimli::Error> {
        if let Some(attr) = entry.attr(at)? {
            if let gimli::AttributeValue::DebugStrRef(offset) = attr.value() {
                Ok(Some(
                    self.parser.dwarf.debug_str.get_str(offset)?.to_string()?,
                ))
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
        let filter = &self.parser.options.filter;
        match filter {
            Filter::SymbolRegex(r) => r.is_match(name),
            Filter::SymbolList(set) => set.get(name).is_some(),
            _ => false,
        }
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
                let string = self.parser.dwarf.debug_str.get_str(offset)?;
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

    // get the typename of a type. if the type is invalid, or is
    // located in a hidden namespace, the None is returned
    fn get_typename(&self, offset: UnitOffset<'i>) -> Option<String> {
	if let Ok(entry) = self.unit.entry(offset) {
            let name = self.get_str_val(&entry, constants::DW_AT_name);
            let namespace = self.ns_ranges.range((Bound::Unbounded, Bound::Included(&offset))).next_back().map(|n| n.1);
	    match entry.tag() {
		constants::DW_TAG_structure_type  => {
                    if let Ok(Some(name)) = name {
                        match namespace {
                        // if we found a namespace, and it is valud
                            Some(Some(namespace)) => {
		                if namespace.len() > 0 {
                                    Some(format!("{}::{}",namespace,name))
		                } else {
                                    Some(String::from(name))
                                }
                            },
                            // we found a namespace, but it is blacklisted
                            Some(None) => None,
                            // we did not a namespace for this symbol
	                    None => Some(String::from(name)),
                        }
                    } else {
                        None
                    }
                },
		constants::DW_TAG_base_type => {
		    if let Ok(name) = name {
			name.map(String::from)
		    } else {
			None
		    }
		},
                constants::DW_TAG_array_type => Some(String::from("?array?")),
		_ => None,
	    } 
        } else {
            None
        }
    }
}
