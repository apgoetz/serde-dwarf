use crate::typ;
use crate::parser::{UnitOffset,Reader,DebugInfoParser, UnitParser, unit_to_di, get_type, get_array_length};
use crate::err::InternalError;

use gimli::constants;
use std::ops::Bound;
use std::result;
use std::io::Write;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::TryFrom;
use indexmap::IndexMap;
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
                offset: gimli::DebugInfoOffset) -> Result<(), InternalError> {
        // if we have visited this type before, we can leave, it is already added 
        if builder.contains_key(offset) {return Ok(());}
        // get the local offset
        let (unit, u_offset) = self.di_to_unit(offset).ok_or(InternalError::new(&format!("Could not convert  DIOffset to Unit Offset: {:?}", offset)))?;
        // if we can build an entry here
        let entry = unit.entry(u_offset).map_err(|e| InternalError::new(&e.to_string()))?;
        let name = self.di_parser.get_str_val(&entry, constants::DW_AT_name)
            .map_err(|e| InternalError::new(&e.to_string()))?
            .ok_or(InternalError::new("Could not read name of type"))?;
        let _namespace = self.get_namespace(offset);

        
        match entry.tag() {
            constants::DW_TAG_base_type => {
                let prim = typ::PrimitiveType::try_from(name).map_err(|e| InternalError::new(&e.to_string()))?;
                builder.add_prim(offset, prim);
            }
            constants::DW_TAG_structure_type => {
                
                // if it is an enum
                if let Some(variants) = self.try_parse_enum(offset) {
                    // different behaviors depending on the enum 
                    if name.starts_with("Option<") {
                        //TODO: also check namespace and number of variants
                        // get the subtypes used by this enum. Options should only have one subtype
                        let mut types = self.get_variants(&variants).map_err(|e| e.extend(&format!("Could not get variants for enum {}", name)))?.1.into_iter();
                        // if it has a different number:
                        if types.size_hint() != (1, Some(1)) {
                            return Err(InternalError::new("Option Type has more than one subtype, perhaps this isn't core::option?"));
                        }
                        // get the type of the generic option
                        let variant = types.next().unwrap();
                        builder.add_option(offset, variant);
                        // need to roll back adding of option
                        if let Err(e) = self.add_type(builder, variant) {
                            builder.remove_key(offset);
                            return Err(e.extend(&format!("Could not parse option {}", name)));
                        } else {return Ok(()); }
                    } else  {
                        // handle the generic enum case
                        let (variants, subtypes) = self.get_variants(&variants)?;
                        builder.add_enum(offset, name, &variants);
                        // add all of the fields in the type
                        for k in subtypes {
                            if let Err(e) = self.add_type(builder, k) {
                                //roll back adding the enum
                                builder.remove_key(offset);
                                return Err(e.extend(&format!("could not add subtype {:?} of enum {}", k, name)));
                            }
                        }
                        return Ok(());                        
                    }

                    // else if it is a struct
                } else if let Some(fields) = self.try_parse_struct(offset) {
                    
                    if fields.0.is_empty() {
                        // handle unit struct
                        builder.add_unit_struct(offset, name);
                    } else if name.starts_with('(') {
                        // if we found a tuple
                        if self.is_tuple(&fields) {
                            builder.add_tuple(offset, &fields.1);
                        } else {
                            // tuple could not be parsed, but the name starts with tuple
                            return Err(InternalError::new("tuple has malformed fields!"));
                        }
                    } else if self.is_tuple(&fields) {
                        // if it has tuple named fields
                        if  fields.0.len() == 1 {
                            //we have a newtype
                            builder.add_newtype(offset, name, fields.1[0]);
                        } else {
                            // we have a tuplestruct
                            builder.add_tuple_struct(offset, name, &fields.1);
                        }
                    } else {
                        // we are handling normal structs. First, handle special cases from core: 
                        match name {
                            "String" | "&str" => {builder.add_prim(offset, typ::PrimitiveType::String); return Ok(());}
                            "&[u8]" => {builder.add_prim(offset, typ::PrimitiveType::ByteArray); return Ok(());}
                            _ => {
                                // we get to the "normal" struct case
                                let map_fields = fields.0.iter().map(|f|String::from(*f)).zip(fields.1.iter().copied()).collect::<IndexMap<_,_>>();
                                builder.add_struct(offset, name, &map_fields);
                            }
                        }
                    }
                    // add all of the fields in the type
                    for (fieldname, offset) in fields.0.iter().zip(fields.1.iter()) {
                        if let Err(e) = self.add_type(builder, *offset) {
                            // roll back adding the struct
                            builder.remove_key(*offset);
                            return Err(e.extend(&format!("could not add field {} of struct {}", fieldname, name)));
                        }
                    }
                    return Ok(());
                } else {
                    return Err(InternalError::new(&format!("cannot add unknown DW_TAG_structure:\n {}", self.dump_type_die(offset))));
                }

                
            },
            constants::DW_TAG_pointer_type => {
                return Err(InternalError::new(&format!("cannot add pointer type:\n{}", self.dump_type_die(offset))));
            }
            _ => {
                return Err(InternalError::new(&format!("cannot add type with tag {:?}", entry.tag())));
            }
        }
        Ok(())
    }

    // returns None if parsing failed
    // returns Ok(true) if we found a tuple
    // returns Ok(false) if we did not find a tuple
    fn add_tuple(&self,
                 builder: &mut typ::TypeBuilder<gimli::DebugInfoOffset>,
                 offset: gimli::DebugInfoOffset,
                 fields: (Vec<&'i str>, Vec<gimli::DebugInfoOffset>)) -> Result<(), InternalError> {

        let fields = fields.1;
        builder.add_tuple(offset, &fields);
        for offset in fields {
            self.add_type(builder, offset).map_err(|e| e.extend("Could not add tuple"))?;
        }
        Ok(())
    }

    
    // returns true if field names match tuple convention
    // returns false if field names do not match tuple convention
    fn is_tuple(&self,
                fields: &(Vec<&'i str>, Vec<gimli::DebugInfoOffset>)) -> bool {
        const MAX_TUPLE : usize = 16;
        // if we have some fields, we may have found a tuple.
        // need to make sure 
        if !fields.0.is_empty() && fields.0.len() <= MAX_TUPLE {

            // check that each field has the right format.
            // for now, rust encodes tuples in debuginfo as a struct with each field of the for __0, __1, etc.
            let tuple_names = (0..MAX_TUPLE).map(|x| format!("__{}", x)).collect::<Vec<_>>();

            for (field, tup_field) in fields.0.iter().zip(tuple_names.iter()) {
                if field != tup_field {
                    // one of these fields does not match: its not a tuple
                    return false;
                }
            }
            return true
        }
        return false
    }

    // returns None if parsing failed
    // returns Some(_) if we found a struct
    fn try_parse_struct(&self,
                     offset: gimli::DebugInfoOffset) -> Option<(Vec<&'i str>, Vec<gimli::DebugInfoOffset>)> {
        let (unit, u_offset) = self.di_to_unit(offset)? ;
        let mut cursor = unit.entries_at_offset(u_offset).ok()? ;
        let mut depth = 0;
        let mut field_names = Vec::new();
        let mut field_vals = Vec::new();
        // skip the first DIE, since that is the struct
        cursor.next_dfs().ok()??;
        while let Some((delta_depth, entry)) = cursor.next_dfs().ok()? {
            depth += delta_depth;
            if depth <= 0 {
                break;
            }
            if entry.tag() == constants::DW_TAG_member {
                field_names.push(self.di_parser.get_str_val(&entry, constants::DW_AT_name).ok()??);
                field_vals.push(get_type(unit, &entry).ok()?);
            }
        }
        Some((field_names, field_vals))
    }

    // returns None if parsing failed
    // returns Some(_) if we found an enum
    fn try_parse_enum(&self,
                     offset: gimli::DebugInfoOffset) -> Option<(Vec<&'i str>, Vec<gimli::DebugInfoOffset>)> {
        let (unit, u_offset) = self.di_to_unit(offset)? ;
        let mut cursor = unit.entries_at_offset(u_offset).ok()? ;
        let mut depth = 0;
        let mut variant_names = Vec::new();
        let mut variant_vals = Vec::new();
        // skip the first DIE, since that is the enum 
        cursor.next_dfs().ok()??;
        // get the next element, which should be the variant description
        if let Some((_, entry)) = cursor.next_dfs().ok()? {
            // make sure we have an enum variant thingy
            if entry.tag() != constants::DW_TAG_variant_part {
                return None;
            }
            // now we can parse the entries
            while let Some((delta_depth, entry)) = cursor.next_dfs().ok()? {
                depth += delta_depth;
                if depth <= 0 {
                    break;
                }
                // if we are at a variant: 
                if entry.tag() == constants::DW_TAG_variant {
                    // the child of this DIE describes the name and type of the variant
                    if let Some((delta_depth, entry)) = cursor.next_dfs().ok()? {
                        depth += delta_depth;
                        variant_names.push(self.di_parser.get_str_val(&entry, constants::DW_AT_name).ok()??);
                        variant_vals.push(get_type(unit, &entry).ok()?);
                    }
                }
            }
            Some((variant_names, variant_vals))
        } else {
            None
        }
    }

    // build a bunch of variants 
    fn get_variants(&self, variants: &(Vec<&'i str>, Vec<gimli::DebugInfoOffset>))
                    -> Result<(Vec<typ::Variant<gimli::DebugInfoOffset>>, HashSet<gimli::DebugInfoOffset>), InternalError>
    {
        let mut results = Vec::with_capacity(variants.0.len());
        let mut subtypes = HashSet::new();
        for (&name, &offset) in variants.0.iter().zip(variants.1.iter()) {
            let name = String::from(name);
            let fields = self.try_parse_struct(offset).ok_or(InternalError::new(&format!("could not get struct fields for struct {}", name)))?; 
            
            if fields.0.is_empty() {
                // handle unit struct
                results.push(typ::Variant::Unit(name));
            } else if self.is_tuple(&fields) {
                // if it has tuple named fields
                if  fields.0.len() == 1 {
                    //we have a newtype
                    results.push(typ::Variant::NewType(name, fields.1[0]));
                    subtypes.insert(fields.1[0]);
                } else {
                    // we have a tuplestruct
                    subtypes.extend(fields.1.iter().copied());
                    results.push(typ::Variant::Tuple(name, fields.1));
                }
            } else {
                // we are handling normal structs. 

                subtypes.extend(fields.1.iter().copied());
                let map_fields = fields.0.iter().map(|f|String::from(*f)).zip(fields.1.iter().copied()).collect::<IndexMap<_,_>>();
                results.push(typ::Variant::Struct(name, map_fields));
            }

        }
        Ok((results, subtypes))
    }
    
    // // constructs a typebuilder and adds  a type
    // fn get_typebuilder(&self,
    //                    offset: gimli::DebugInfoOffset)
    //                    -> Option<typ::TypeBuilder<gimli::DebugInfoOffset>> {
    //     let mut builder = typ::TypeBuilder::new();
    //     self.add_type(&mut builder, offset)?;
    //     Some(builder)
    // }
    
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
    ) -> String {
        let mut buf = Vec::<u8>::new();
        let namespace = self.get_namespace(offset);
        if let Some((unit, offset)) = self.di_to_unit(offset) {
            if let Ok(mut cursor) = unit.entries_at_offset(offset) {
                let mut depth = 0;
                if let Some(Some(namespace)) = namespace {
                    writeln!(buf, "namespace: {}", namespace).ok();
                }

                // dump the first entry
                if let Ok(Some((delta_depth, entry))) = cursor.next_dfs() {
                    depth += delta_depth;
                    self.di_parser.dump_die(&mut buf, depth as usize, &entry).ok();
                    // dump child entries
                    while let Ok(Some((delta_depth, entry))) = cursor.next_dfs() {
                        depth += delta_depth;
                        if depth <= 0 {
                            break;
                        }
                        self.di_parser.dump_die(&mut buf, depth as usize, &entry).ok();
                    }
                }
            }
        }
        //print the DIE, if somehow there is non-utf8 bytes , we just replace with a placeholder
        String::from_utf8(buf).unwrap_or(String::from("???"))
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
