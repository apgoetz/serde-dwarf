#![allow(dead_code)]

use core::result;
use std::error;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
//use std::ops::Deref;
use gimli::{self};
use memmap;
use object;
use regex;
use std::collections::{HashMap, HashSet, hash_map};
//use fallible_iterator::FallibleIterator;
mod parser;
mod typ;
pub use typ::Type;
#[derive(Debug)]
enum ErrorCode {
    Io(io::Error),
    Object(object::Error),
    Gimli(gimli::Error),
    Builder(BuilderError),
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorCode::Io(e) => fmt::Display::fmt(e, f),
            ErrorCode::Object(e) => fmt::Display::fmt(e, f),
            ErrorCode::Gimli(e) => fmt::Display::fmt(e, f),
            ErrorCode::Builder(e) => fmt::Display::fmt(e, f),
        }
    }
}

#[derive(Debug)]
struct ErrorImpl {
    code: ErrorCode,
}

// error type for this crate
pub struct Error(ErrorImpl);

impl Error {
    fn io(e: io::Error) -> Self {
        Error(ErrorImpl {
            code: ErrorCode::Io(e),
        })
    }

    fn object(e: object::Error) -> Self {
        Error(ErrorImpl {
            code: ErrorCode::Object(e),
        })
    }
}

impl From<gimli::Error> for Error {
    fn from(err: gimli::Error) -> Self {
        Error(ErrorImpl {
            code: ErrorCode::Gimli(err),
        })
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error(ErrorImpl {
            code: ErrorCode::Io(err),
        })
    }
}


impl From<object::Error> for Error {
    fn from(err: object::Error) -> Self {
        Error(ErrorImpl {
            code: ErrorCode::Object(err),
        })
    }
}

// redefine result for easy usage
type Result<T> = result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.code.fmt(f)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None // currently, none of our errors are connected to other errors
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, fmt)
    }
}

// represents any valid value that could be deserialized based on dwarf data
// similar to serde_json::Value, or rmpv::Value.
enum Value {}

// wraps a non-self describing deserializer and and implements deserialize_any, using the type information to drive the underlying deserializer
//
struct TypeDeserializer {}

enum Filter {
    NoFilter,
    SymbolRegex(regex::Regex),
    SymbolList(HashSet<String>),
    TypeRegex(regex::Regex),
    TypeList(HashSet<String>),
}

#[derive(Debug, Clone)]
struct BuilderError(regex::Error);

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

pub struct DebugInfoBuilder {
    // should type signatures be tokenized, or left as strings
    store_tokens: bool,
    // regex to match the
    filter: Filter,
    // did an error occur during construction
    error: Option<BuilderError>,
    // should compressed data be supported
    compressed: bool,
}

impl DebugInfoBuilder {
    /// make a builder
    pub fn new() -> Self {
        DebugInfoBuilder {
            store_tokens: false,
            filter: Filter::NoFilter,
            error: None,
            compressed: false,
        }
    }

    /// cache types as tokenstreams instead of strings.
    /// if not selected, lookups for types must match the string in the debug info.
    /// could slow down performance, not default
    pub fn store_parsed<'a>(&'a mut self) -> &'a DebugInfoBuilder {
        self.store_tokens = true;
        self
    }

    /// cache types as strings. Default behavior. Opposite of `store_parsed()`
    /// lookups must match exactly the string names in the debuginfo
    pub fn store_raw<'a>(&'a mut self) -> &'a DebugInfoBuilder {
        self.store_tokens = false;
        self
    }

    /// specify regex that types must match in order to limit extracted types.
    /// Filters are not chained, only one filter is allowed
    pub fn filter_type_regex<'a>(&'a mut self, r: &str) -> &'a DebugInfoBuilder {
        self.filter = match regex::Regex::new(r) {
            Ok(r) => Filter::TypeRegex(r),
            Err(e) => {
                self.error = Some(BuilderError(e));
                Filter::NoFilter
            }
        };
        self
    }

    /// specfiy a list of strings. types in debuginfo must match one
    /// of the listed strings in order to be extracted. Type must also
    /// implement serde::Serialize
    pub fn filter_type_list<'a, I>(&'a mut self, i: I) -> &'a DebugInfoBuilder
        where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        self.filter = Filter::TypeList(i.into_iter().map(|s| String::from(s.as_ref())).collect());
        self
    }

    /// specify regex that symbols must match in order to limit extracted types.
    /// Filters are not chained, only one filter is allowed
    pub fn filter_sym_regex<'a>(&'a mut self, r: &str) -> &'a DebugInfoBuilder {
        self.filter = match regex::Regex::new(r) {
            Ok(r) => Filter::SymbolRegex(r),
            Err(e) => {
                self.error = Some(BuilderError(e));
                Filter::NoFilter
            }
        };
        self
    }

    /// specfiy a list of strings. symbols in debuginfo must match one of
    /// the listed strings in order to be extracted
    pub fn filter_sym_list<'a, I>(&'a mut self, i: I) -> &'a DebugInfoBuilder
        where
        I: IntoIterator,
        I::Item: AsRef<str>,

    {
        self.filter = Filter::SymbolList(i.into_iter().map(|s| String::from(s.as_ref())).collect());
        self
    }

    /// Parse file located at the given path
    pub fn parse_path<P: AsRef<Path>>(&self, path: P) -> Result<DebugInfo> {
        let file = fs::File::open(path).map_err(|e| Error::io(e))?;
        self.parse_file(&file)
    }

    /// Parse the data in an opened file handle
    pub fn parse_file(&self, file: &fs::File) -> Result<DebugInfo> {
        let mmap = unsafe { memmap::Mmap::map(&file).map_err(|e| Error::io(e))? };
        self.parse_bytes(&mmap)
    }

    /// parse the data in a byte slice
    pub fn parse_bytes(&self, bytes: &[u8]) -> Result<DebugInfo> {
        let object = object::File::parse(bytes).map_err(|e| {
            Error(ErrorImpl {
                code: ErrorCode::Object(e),
            })
        })?;
        self.parse_object(&object)
    }

    /// parse the data in an existing object
    pub fn parse_object<'input>(&self, object: &'input object::File<'input>) -> Result<DebugInfo> {
        // if the builder had an error during construction, return it now
        if let Some(e) = &self.error {
            return Err(Error(ErrorImpl {
                code: ErrorCode::Builder(e.clone()),
            }));
        }

        if self.compressed {
            todo!();
        }

        let parser = parser::DebugInfoParser::new(self, object)?;
        parser.parse()
    }

    /// allow parsing compressed debug info.
    /// This requires the debuginfo sections to be copied out, which could slow processing down
    /// Defaults to not allow compress
    pub fn allow_compressed<'a>(&'a mut self) -> &'a DebugInfoBuilder {
        self.compressed = true;
        self
    }
    /// do not allow parsing compressed debug info.  debug info can be
    /// referenced directly from mmap'ed file, potentially speeding up
    /// processing. compressed debug info may fail silently. default
    /// behavior. opposite of `allow_compressed()`
    pub fn deny_compressed<'a>(&'a mut self) -> &'a DebugInfoBuilder {
        self.compressed = false;
        self
    }

    fn allow_namespace(&self, ns: &str) -> bool {
        let denylist = ["{{impl}}", "{{closure}}"];

        // check to see if this namespace contains a forbidden namespace
        for deny in denylist.iter() {
            if ns.contains(deny) {
                return false;
            }
        }
        true
    }
}


pub struct Iter<'a>(hash_map::Keys<'a, String, gimli::DebugInfoOffset>);

impl Clone for Iter<'_> {
    #[inline]
    fn clone(&self) -> Self {
        Iter(self.0.clone())
    }
}

impl fmt::Debug for Iter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        self.0.next().map(String::as_str)
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl ExactSizeIterator for Iter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.0.len()
    }
}

/// represents a parsed  debug info, that types can be found in
pub struct DebugInfo {
    types: HashMap<String, gimli::DebugInfoOffset>,
    syms: HashMap<String, gimli::DebugInfoOffset>,
    symtypes: HashMap<String, String>,
    builder: typ::TypeBuilder<gimli::DebugInfoOffset>
}

impl DebugInfo {
    /// get a symbol from the symbolcache
    pub fn get_by_sym(&self, symbol: &str) -> Option<Type> {
        self.builder.build(*self.syms.get(symbol)?)
    }
    /// lookup via symbols in the file
    pub fn get_by_type(&self, symbol: &str) -> Option<Type> {
        self.builder.build(*self.types.get(symbol)?)
    }
    /// get an iterator of the symbols we know
    pub fn syms(&self) ->  Iter {
        Iter(self.syms.keys())
    }
    
    /// get an iterator of the symbols we know
    pub fn symtype(&self, symbol: &str) ->  Option<String> {
        self.symtypes.get(symbol).map(String::from)
    }

    /// get an iterator of the types we know
    pub fn types(&self) ->  Iter {
        Iter(self.types.keys())
    }
}

// represents a symbol located in the debugging info. Symbols are
// currently only namespaced by their module path, they dont contain
// the full scope path. Each symbol has a type
struct Symbol {}

// represents the module path a symbol or type lives under.  symbols
// can be located further in scope blocks, but for now, they only
// support accessing via the module hierarchy
struct ModulePath {}

#[cfg(test)]
mod tests {
    //    use std::env;
    //    use crate::DebugInfo;
    #[test]
    fn it_works() {}
}

// determining if a type implements Serialize:::
// DWARF does not represent trait info, so we have to determine from functions in the trait implementation:
// look for DW_TAG_subprogram DIEs
// they will have two attributes we can use, but they need to be demangled
//
// DW_AT_linkage_name describes the function as part of the trait:
//
// DW_AT_linkage_name          serde::ser::impls::<impl serde::ser::Serialize for &T>::serialize
//
//
// DW_AT_name is the monomorphized function name, it will include the serializer:
//
// DW_AT_name                  serialize<(i32, i32, i32),&mut serde_json::ser::Serializer<std::io::stdio::Stdout, serde_json::ser::CompactFormatter>>
//
// You can determine the function implements serialize if it starts with: serde::ser::impls<impl serde::ser::Serialize for ???>
//
// the type that the function serializes should be the first type parameter of DW_AT_name
//
// You can also use the first parameter of the function (self) but
// keep in mind this takes a pointer to the conrete data type, which
// ends up being a different type in the debuginfo, so you cant link
// them back together (no use just using unitoffset to connect them

// parsing arrays
//
// starts as array type.
//
// the array type has a DW_AT_type attribute which
// points to the type of the elment. (standard unit offset value)
//
// there is also a nested DIE record with a tag of
// DW_TAG_subrange_type.
//
// This type contains a DW_AT_type with a pointer
// to the type of the index variable, which seems
// to be hardcoded as __ARRAY_SIZE_TYPE__
//
// The DW_TAG_subrange_type also contains a DW_AT_count,
// which is the number of elements in the array
