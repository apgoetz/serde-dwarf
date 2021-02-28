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
fn test_unit_struct() {
    // serde_derive expects a unit type for unit structs
    #[derive(Deserialize)]
    struct FooNewType;
    assert_deserialize!(test_unit_struct, FooNewType);
}

#[test]
fn test_newtype_struct() {

    #[derive(Deserialize)]
    struct Foo(u8);
    assert_deserialize!(test_newtype_foo, Foo);


    // test that worlds stupidest linked list works
    // #[derive(Deserialize)]
    // struct List(Option<Box<List>>);

    // assert_deserialize!(test_empty_ll, List, 0);
    // assert_deserialize!(test_full_ll, List, 1);
    
    
}

#[test]
fn test_tuple() {
    assert_deserialize!(test_2_tuple, (u8,u16));
}

#[test]
fn test_tuple_struct() {
    #[derive(Deserialize)]
    struct Foo(u8,u16);
    assert_deserialize!(test_tuple_struct, Foo);
}

#[test]
fn test_struct() {
    #[derive(Deserialize)]
    struct Foo {
        _a: u8, 
        _b: u16,
    }

    // this struct looks like a tuple at first, but its not
    #[derive(Deserialize)]
    struct AlmostTuple {
        __0: u8, 
        _b: u16,
    }

    // make sure that even if fields are in an arbitary unsorted order, we properly check for them
    #[derive(Deserialize)]
    struct Unsorted {
        _1: u8, 
        _b: i8, 
        _a: u16,
        _bb: String,
    }
    
    assert_deserialize!(test_struct, Foo);
    assert_deserialize!(test_almost_tuple, AlmostTuple);
    assert_deserialize!(test_unsorted_struct, Unsorted);
    
    
}


#[test]
fn test_option() {
    assert_deserialize!(test_option_none, Option<u8>);
    assert_deserialize!(test_option_some, Option<u8>, 1);
}

#[test]
fn test_enum() {
    #[derive(Deserialize)]
    enum Foo {
        A,
        B(u8),
        C(u8,u16),
        D {
            _a: u32
        }
    }

    assert_deserialize!(test_enum_var_a, Foo, 0);
    assert_deserialize!(test_enum_var_b, Foo, 1);
    assert_deserialize!(test_enum_var_c, Foo, 2);
    assert_deserialize!(test_enum_var_d, Foo, 3);

}
