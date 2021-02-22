use crate::typ;
use crate::parser::{UnitOffset,Reader,DebugInfoParser, UnitParser, unit_to_di, get_type, get_array_length};
use gimli::constants;
use std::ops::Bound;
use std::result;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::TryFrom;

// struct to build types from debug entries

// contains the results of all of our unit parsing
pub struct UnitResults<'i> {
    // mapping of parser results to the sections of the file they came from
    pub namespaces: BTreeMap<gimli::DebugInfoOffset, Option<String>>,
    pub units: BTreeMap<gimli::DebugInfoOffset, &'i gimli::Unit<Reader<'i>>>,
    // reference to the root parser (contains the options and dwarf object
    pub di_parser: &'i DebugInfoParser<'i>,
    // symbols we want to export
    pub symbol_types: HashMap<&'i str, gimli::DebugInfoOffset>,
    // info on serializable types we want to support
    pub  serialize_types: HashSet<gimli::DebugInfoOffset>,
}

impl<'i> UnitResults<'i> {
    pub fn new(di_parser: &'i DebugInfoParser<'i>) -> Self {
        UnitResults {
            di_parser,
            namespaces: BTreeMap::new(),
            units: BTreeMap::new(),
            symbol_types: HashMap::new(),
            serialize_types: HashSet::new(),
        }
    }
    
    pub fn add_unit(&mut self, unit: UnitParser<'i>) {
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
    pub fn add_type(&self,
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
            constants::DW_TAG_structure_type => {
                // if we found a tuple
                if self.try_add_tuple(builder, offset)? {
		    return Some(());
                } 

                // otherwise, try to parse as something else only handle the rest of the primitive types
                match name.ok()?? {
                    "String" => {builder.add_prim(offset, typ::PrimitiveType::String);}
                    "&str" => {builder.add_prim(offset, typ::PrimitiveType::String);}
                    "&[u8]" => {builder.add_prim(offset, typ::PrimitiveType::ByteArray);}
                    _ => {
                        eprintln!("cannot add unknown struct!");
                        self.dump_type_die(offset);
                        return None;
                    }
                }
            },
            _ => {return None;}
        }
        Some(())
    }

    // returns None if parsing failed
    // returns Ok(true) if we found a tuple
    // returns Ok(false) if we did not find a tuple
    fn try_add_tuple(&self,
                     builder: &mut typ::TypeBuilder<gimli::DebugInfoOffset>,
                     offset: gimli::DebugInfoOffset) -> Option<bool> {
        let (unit, u_offset) = self.di_to_unit(offset)? ;
        let mut cursor = unit.entries_at_offset(u_offset).ok()? ;
        let mut depth = 0;
        let mut fields = Vec::new();
        // skip the first DIE, since that is the struct
        cursor.next_dfs().ok()??;
        while let Some((delta_depth, entry)) = cursor.next_dfs().ok()? {
            depth += delta_depth;
            if depth < 0 {
                break;
            }
            let field_name = self.di_parser.get_str_val(&entry, constants::DW_AT_name).ok()??;
            // tuples always consist of a bunch of fields looking like __0, __1, __2, etc.
            if entry.tag() == constants::DW_TAG_member && field_name == format!("__{}", fields.len()) {
                // add the field of this tuple
                fields.push(get_type(unit, &entry).ok()?);
            } else {
		// we found something valid that is not a tuple
                return Some(false);
            }
        }
        // if we have some fields, we have found a tuple
        if !fields.is_empty() {
            builder.add_tuple(offset, &fields);
            for offset in fields {
                if self.add_type(builder, offset).is_none() {
                    eprintln!("could not add tuple field!");
		    return None;
                }
            }
            return Some(true)
        }
        return None
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
    pub fn subfunction_to_self(
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

    fn dump_type_die (
        &self,
        offset: gimli::DebugInfoOffset,
    ) {
        let namespace = self.get_namespace(offset);
        if let Some((unit, offset)) = self.di_to_unit(offset) {
            if let Ok(mut cursor) = unit.entries_at_offset(offset) {
                let mut depth = 0;
                if let Some(Some(namespace)) = namespace {
                    eprintln!("namespace: {}", namespace);
                }
                while let Ok(Some((delta_depth, entry))) = cursor.next_dfs() {
                    depth += delta_depth;
                    if depth < 0 {
                        return;
                    }
                    self.di_parser.dump_die(depth as usize, &entry).ok();
                }
            }
        }
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
                Some((unit, offset))
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

    pub fn get_typename(&self, offset: gimli::DebugInfoOffset) -> Option<String> {
        if let Some((name, namespace)) = self.get_fullname(offset) {
            if !namespace.is_empty() {
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

                        if !namespace.is_empty() {
                            Some((format!("[{}::{}; {}]", namespace, name, len), String::new()))
                        } else {
                            Some((format!("[{}; {}]", name, len), String::new()))
                        }
                    },
                    _ => {
                        if let Ok(Some(name)) = name {
                            eprintln!("unknown type: {}", name);
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
