// test that Type deserializes to the same tokens as serde deserialize
// impls

use serde::{Deserialize};

mod common;
use common::{MockDeserializer};
use linkme::distributed_slice;
use lazy_static::lazy_static;
use serde_dwarf::{DebugInfo, DebugInfoBuilder};
use serde::de::DeserializeSeed;
use std::collections::HashSet;

#[distributed_slice]
static TYPES: [&'static str] = [..];

lazy_static! {
    static ref DEBUGINFO: DebugInfo = {

	// first we check that we dont have any duplicates in TYPES
	// distributed slice. Duplicates means that one of the below assert calls accidentally has the same value twice
	let mut seen = HashSet::new();

	TYPES.iter().for_each(|t| if ! seen.insert(t) {panic!("Duplicate types specified in asserts!")});

	
	DebugInfoBuilder::new()
        .filter_sym_list(TYPES)
            .parse_path(std::env::current_exe().unwrap()).unwrap()
    };
        
}

macro_rules! assert_deserialize {
    ($symname:tt, $typ:ty, $magic:expr) => {
	{
        const TYPE_STR: &str = stringify!($symname);
        #[distributed_slice(TYPES)]
        static MY_TYPE: &str = TYPE_STR;

        let mut serde_result = Vec::new();
	#[allow(unused_variables)]
        let $symname = 
            <$typ as Deserialize>::deserialize(MockDeserializer::new(&mut serde_result, $magic)).unwrap();

        let mut dwarf_result = Vec::new();
        let typ = DEBUGINFO.sym(TYPE_STR).expect(format!("could not build Type for symbol: {} type is {}", TYPE_STR, DEBUGINFO.symtype(TYPE_STR).unwrap()).as_str());
            typ.deserialize(MockDeserializer::new(&mut dwarf_result, $magic)).unwrap();
	}
    };
    ($symname:tt, $typ:ty) => {
	assert_deserialize!($symname, $typ, 0)
    };
}

#[test]
fn test_prims() {

    assert_deserialize!(test_u8, u8);
    assert_deserialize!(test_u16, u16);
    assert_deserialize!(test_u32, u32);
    assert_deserialize!(test_u64, u64);
    assert_deserialize!(test_usize, usize);
    assert_deserialize!(test_u128, u128);

    assert_deserialize!(test_i8, i8);
    assert_deserialize!(test_i16, i16);
    assert_deserialize!(test_i32, i32);
    assert_deserialize!(test_i64, i64);
    assert_deserialize!(test_isize, isize);
    assert_deserialize!(test_i128, i128);

    
    assert_deserialize!(test_bool, bool);
    assert_deserialize!(test_unit, ());
    assert_deserialize!(test_char, char);

    assert_deserialize!(test_string, String);
    assert_deserialize!(test_str, &str);
    assert_deserialize!(test_u8_slice, &[u8]);

}

#[test]
fn test_tuple() {
    assert_deserialize!(test_2_tuple, (u8,u16));
}


#[test]
fn test_option() {
    assert_deserialize!(test_option_none, Option<u8>);
    assert_deserialize!(test_option_some, Option<u8>, 1);
}

