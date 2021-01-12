use gimli::{self};
use crate::{Result, Error, ErrorImpl, ErrorCode, DebugInfo, DebugInfoBuilder};
use std::result;
use std::collections::{HashMap};
use object::{self,Object, ObjectSection};
use fallible_iterator::FallibleIterator;



// trait Reader: gimli::Reader<Offset = usize> + Send + Sync {}
// impl<'input, Endian> Reader for gimli::EndianSlice<'input, Endian> where
//     Endian: gimli::Endianity + Send + Sync
// {
// }
type Reader<'i> = gimli::EndianSlice<'i,gimli::RunTimeEndian>;

pub struct DebugInfoParser<'i> {
    object: &'i object::File<'i>,
    dwarf: gimli::Dwarf<Reader<'i>>,
    options: &'i DebugInfoBuilder
}


impl<'i> DebugInfoParser<'i> {
        /// parse the data in an existing object 
    pub fn new(options: &'i DebugInfoBuilder, object: &'i object::File<'i>) -> Result<Self>
    {

        // most of the code in this function is based on the simple.rs example in the gimli crate
        let endian = if object.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        let load_section = |id: gimli::SectionId| -> result::Result<gimli::EndianSlice<'i,gimli::RunTimeEndian>, gimli::Error> {
            let empty = Ok(gimli::EndianSlice::new(&[], endian));
            match object.section_by_name(id.name()) {
                Some(ref section) => {
                    if let Ok(data) = section.compressed_data() {
                        if data.format == object::CompressionFormat::None {
                            return Ok(gimli::EndianSlice::new(&data.data, endian))
                        }
                    }
                    empty
                },
                None => empty,
            }
        };
        // Load a supplementary section. We don't have a supplementary object file,
        // so always return an empty slice.
        let load_section_sup = |_| Ok(gimli::EndianSlice::new(&[],endian));

        // Load all of the sections.
        let dwarf = gimli::Dwarf::load(&load_section, &load_section_sup);
        let dwarf = dwarf.map_err(|e| Error(ErrorImpl{code : ErrorCode::Gimli(e)}))?;

        Ok(DebugInfoParser{object,
                           dwarf,
                           options})
    }

    pub fn parse(&self) -> Result<DebugInfo> {

        let types = HashMap::new();
        let syms = HashMap::new();

        // try to get the units from the dwarf file, or print error
        let _result = self.dwarf.units()
            .map(|h| self.dwarf.unit(h))
            .map(|u|self.parse_unit(&u)).collect::<Vec<()>>()?;
        
        Ok(DebugInfo{types, syms})

    }

    // helper function to find matching symbols and types in a unit
    fn parse_unit(&self, unit: &gimli::Unit<Reader<'i>>) -> result::Result<(), gimli::Error> {
        let mut entries = unit.entries();
        let mut depth = 0;
        let mut padding;
        while let Some((delta_depth, entry)) = entries.next_dfs()? {
            depth += delta_depth;
            assert!(depth >= 0);

            
                        
            padding = "  ".repeat(depth as usize);
            
            println!("{}<{}>{}", padding, depth, entry.tag().static_string().unwrap());
            self.dump_attrs(&padding, entry.attrs())?;
        }
        Ok(())
    }

    fn dump_attrs<'abbrev, 'me, 'unit>(
        &self,
        depth: &str,
        mut attrs:  gimli::read::AttrsIter<'abbrev, 'me, 'unit, Reader<'i>>,
        ) -> result::Result<(), gimli::Error> {
    
        while let Some(attr) = attrs.next()? {
            print!("{}", depth);

            if let Some(n) = attr.name().static_string() {
                print!("{}: ", n);
            } else {
                print!("{:27}: ", attr.name());
            }

            if let gimli::AttributeValue::DebugStrRef(offset) = attr.value() {
                let string = self.dwarf.debug_str.get_str(offset)?;
                let string = string.to_string()?;
                println!("{}", string);
            } else if let Some(num) = attr.udata_value() {
                println!("{}", num);
            } else if let gimli::AttributeValue::UnitRef(offset) = attr.value() {
                println!("unit+{:#x}", offset.0);
            } else {
                println!("??? {:?}", attr.value());
            }
        }
        Ok(())
    }
}
