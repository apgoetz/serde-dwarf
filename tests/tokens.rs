// tests that use serde_token
//use serde_dwarf::{self, DebugInfoBuilder};

use serde::{Deserialize};
use std::collections::HashMap;

mod common;
use common::{Tokens, MockDeserializer};

macro_rules! assert_tokens {
    ($typ:ty, $tokens:expr, $magic:expr) => {
        let mut results = Vec::new();
        <$typ as Deserialize>::deserialize(MockDeserializer::new(&mut results, $magic)).unwrap();
        assert_eq!(results, $tokens);
    };
    ($typ:ty, $tokens:expr) => {
        assert_tokens!($typ, $tokens, 0);
    }
}

//use std::env;
#[test]
fn test_primitive() {
    assert_tokens!(u8, [Tokens::U8]);
    assert_tokens!(u16, [Tokens::U16]);
    assert_tokens!(u32, [Tokens::U32]);
    assert_tokens!(u64, [Tokens::U64]);
    assert_tokens!(u128, [Tokens::U128]);

    assert_tokens!(i8, [Tokens::I8]);
    assert_tokens!(i16, [Tokens::I16]);
    assert_tokens!(i32, [Tokens::I32]);
    assert_tokens!(i64, [Tokens::I64]);
    assert_tokens!(i128, [Tokens::I128]);

    assert_tokens!(bool, [Tokens::Bool]);
    assert_tokens!((), [Tokens::Unit]);
    assert_tokens!(char, [Tokens::Char]);

    assert_tokens!(String, [Tokens::String]);
    assert_tokens!(&str, [Tokens::Str]);
    assert_tokens!(&[u8], [Tokens::Bytes]);

    // on ALL platforms, isize/usize are 64 bit
    assert_tokens!(usize, [Tokens::U64]);
    assert_tokens!(isize, [Tokens::I64]);

}

#[test]
fn test_option() {
    assert_tokens!(Option<u8>, [Tokens::Option]);
    assert_tokens!(Option<u8>, [Tokens::Option, Tokens::U8], 1);
}


#[test]
fn test_string() {
    use std::ffi::{CString, OsString, CStr};
    assert_tokens!(CString, [Tokens::ByteBuf]);
    assert_tokens!(Box<CStr>, [Tokens::ByteBuf]);
    assert_tokens!(Box<str>, [Tokens::String]);

    assert_tokens!(OsString, [
        Tokens::Enum("OsString", &["Unix", "Windows"]),
        Tokens::NewTypeVariant("Unix"),
        Tokens::Seq
    ], 0);

    // assert_tokens!(OsString, [
    //     Tokens::Enum("OsString", &["Unix", "Windows"]),
    //     Tokens::NewTypeVariant("Windows"),
    //     Tokens::Seq
    // ], 1);

}
#[test]
fn test_cmp() {
    use std::cmp::Reverse;
    assert_tokens!(Reverse<u8>, [Tokens::U8]);
}

#[test]
fn test_marker() {
    use std::marker::PhantomData;
    assert_tokens!(PhantomData<u8>, [Tokens::UnitStruct("PhantomData")]);
}

#[test]
fn test_seq_collections() {
    use std::collections::{BinaryHeap, BTreeSet, LinkedList,
                           HashSet, VecDeque};
    assert_tokens!(BinaryHeap<u8>, [Tokens::Seq]);
    assert_tokens!(BTreeSet<u8>, [Tokens::Seq]);
    assert_tokens!(LinkedList<u8>, [Tokens::Seq]);
    assert_tokens!(HashSet<u8>, [Tokens::Seq]);
    assert_tokens!(VecDeque<u8>, [Tokens::Seq]);
    assert_tokens!(Vec<u32>, [Tokens::Seq]);
}

#[test]
fn test_map_collections() {
    use std::collections::{BTreeMap, HashMap};
    assert_tokens!(BTreeMap<u32, u64>, [Tokens::Map]);
    assert_tokens!(HashMap<u32, u64>, [Tokens::Map]);
}

#[test]
fn test_ip_addr() {
    use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
    assert_tokens!(IpAddr,  [
        Tokens::Enum("IpAddr", &["V4", "V6"]),
        Tokens::NewTypeVariant("V4"),
        Tokens::Tuple(4),
        Tokens::U8,
        Tokens::U8,
        Tokens::U8,
        Tokens::U8], 0);

    assert_tokens!(IpAddr,  [
        Tokens::Enum("IpAddr", &["V4", "V6"]),
        Tokens::NewTypeVariant("V6"),
        Tokens::Tuple(16),
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,], 1);
    
    assert_tokens!(Ipv4Addr,  [
        Tokens::Tuple(4),
        Tokens::U8,
        Tokens::U8,
        Tokens::U8,
        Tokens::U8]);

    assert_tokens!(Ipv6Addr,  [
        Tokens::Tuple(16),
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,]);

}

#[test]
fn test_socket_addr() {
    use std::net::{SocketAddr, SocketAddrV4, SocketAddrV6};
    assert_tokens!(SocketAddr,  [
        Tokens::Enum("SocketAddr", &["V4", "V6"]),
        Tokens::NewTypeVariant("V4"),
        Tokens::Tuple(2),
        Tokens::Tuple(4),
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U16], 0);

    assert_tokens!(SocketAddr,  [
        Tokens::Enum("SocketAddr", &["V4", "V6"]),
        Tokens::NewTypeVariant("V6"),
        Tokens::Tuple(2),
        Tokens::Tuple(16),
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U16], 1);

    assert_tokens!(SocketAddrV4,  [
        Tokens::Tuple(2),
        Tokens::Tuple(4),
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U16, ]);

    assert_tokens!(SocketAddrV6,  [
        Tokens::Tuple(2),
        Tokens::Tuple(16),
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U8, Tokens::U8, Tokens::U8, Tokens::U8,
        Tokens::U16]);

    
}

#[test]
fn test_path() {
    use std::path::{Path, PathBuf};
    assert_tokens!(&Path, [Tokens::Str]);
    assert_tokens!(Box<Path>, [Tokens::String]);
    assert_tokens!(PathBuf, [Tokens::String]);
}

#[test]
fn test_smart_pointers() {
    use std::sync;
    assert_tokens!(sync::Arc<u8>, [Tokens::U8]);
    assert_tokens!(sync::Weak<u8>, [Tokens::Option], 0);
    assert_tokens!(sync::Weak<u8>, [Tokens::Option, Tokens::U8], 1);

    use std::rc;
    assert_tokens!(rc::Rc<u8>, [Tokens::U8]);
    assert_tokens!(rc::Weak<u8>, [Tokens::Option], 0);
    assert_tokens!(rc::Weak<u8>, [Tokens::Option, Tokens::U8], 1);

    
    assert_tokens!(Box<bool>, [Tokens::Bool]);
    assert_tokens!(Box<[bool]>, [Tokens::Seq]);

    use std::borrow::Cow;
    assert_tokens!(Cow<bool>, [Tokens::Bool]);

    use std::cell::{Cell, RefCell};
    assert_tokens!(Cell<bool>, [Tokens::Bool]);
    assert_tokens!(RefCell<bool>, [Tokens::Bool]);
}

#[test]
fn test_sync() {
    use std::sync::{Mutex, RwLock};
    assert_tokens!(Mutex<bool>, [Tokens::Bool]);

    assert_tokens!(RwLock<bool>, [Tokens::Bool]);

}

#[test]
fn test_time() {
    use std::time::{Duration, SystemTime};
    assert_tokens!(Duration, [
        Tokens::Struct("Duration", &["secs", "nanos"]),
        Tokens::U64,
        Tokens::U32,
    ]);

    assert_tokens!(SystemTime, [
        Tokens::Struct("SystemTime", &["secs_since_epoch", "nanos_since_epoch"]),
        Tokens::U64,
        Tokens::U32,
    ]);
}

#[test]
fn test_ops() {
    use std::ops::{Range, RangeInclusive, Bound};
    assert_tokens!(Range<u8>, [
        Tokens::Struct("Range", &["start", "end"]),
        Tokens::U8,
        Tokens::U8,
    ]);

    assert_tokens!(RangeInclusive<u8>, [
        Tokens::Struct("RangeInclusive", &["start", "end"]),
        Tokens::U8,
        Tokens::U8,
    ]);


    assert_tokens!(Bound<u8>, [
        Tokens::Enum("Bound", &["Unbounded", "Included", "Excluded"]),
        Tokens::UnitVariant("Unbounded"),
    ], 0);

    assert_tokens!(Bound<u8>, [
        Tokens::Enum("Bound", &["Unbounded", "Included", "Excluded"]),
        Tokens::NewTypeVariant("Included"),
        Tokens::U8,
        ], 1);

    assert_tokens!(Bound<u8>, [
        Tokens::Enum("Bound", &["Unbounded", "Included", "Excluded"]),
        Tokens::NewTypeVariant("Excluded"),
        Tokens::U8,
    ], 2);
}

#[test]
#[should_panic]
fn test_nonzero_panics() {
    use std::num;
    assert_tokens!(num::NonZeroU8, [Tokens::U8]);
    
}

#[test]
fn test_num() {
    use std::num;
    assert_tokens!(num::NonZeroU8, [Tokens::U8], 1);
    assert_tokens!(num::NonZeroU16, [Tokens::U16], 1);
    assert_tokens!(num::NonZeroU32, [Tokens::U32], 1);
    assert_tokens!(num::NonZeroU64, [Tokens::U64], 1);
    assert_tokens!(num::NonZeroUsize, [Tokens::U64], 1);
    assert_tokens!(num::NonZeroU128, [Tokens::U128], 1);

    
    assert_tokens!(num::NonZeroI8, [Tokens::I8], 1);
    assert_tokens!(num::NonZeroI16, [Tokens::I16], 1);
    assert_tokens!(num::NonZeroI32, [Tokens::I32], 1);
    assert_tokens!(num::NonZeroI64, [Tokens::I64], 1);
    assert_tokens!(num::NonZeroIsize, [Tokens::I64], 1);
    assert_tokens!(num::NonZeroI128, [Tokens::I128], 1);
    
}

#[test]
fn test_unit_struct() {
    // serde_derive expects a unit type for unit structs
    #[derive(Deserialize)]
    struct Foo;
    assert_tokens!(Foo, [Tokens::UnitStruct("Foo")]);
}

#[test]
fn test_newtype_struct() {

    #[derive(Deserialize)]
    struct Foo(u8);
    assert_tokens!(Foo, [Tokens::NewTypeStruct("Foo"), Tokens::U8]);


    // test that worlds stupidest linked list works
    #[derive(Deserialize)]
    struct List(Option<Box<List>>);
    assert_tokens!(List, [
        Tokens::NewTypeStruct("List"),
        Tokens::Option,
    ], 0);

    
    assert_tokens!(List, [
        Tokens::NewTypeStruct("List"),
        Tokens::Option,
        Tokens::NewTypeStruct("List"),
        Tokens::Option,
    ], 1);

    
}

#[test]
fn test_tuple() {
    assert_tokens!((u8,u16), [Tokens::Tuple(2), Tokens::U8, Tokens::U16]);
}


#[test]
fn test_seq() {
    assert_tokens!(Vec<u16>, [Tokens::Seq]);
    assert_tokens!(Vec<u16>, [
        Tokens::Seq,
        Tokens::U16,
        Tokens::U16,
    ], 2);
}

#[test]
fn test_tuple_struct() {
    #[derive(Deserialize)]
    struct Foo(u8,u16);
    assert_tokens!(Foo, [Tokens::TupleStruct("Foo",2),
                         Tokens::U8,
                         Tokens::U16]);
}

#[test]
fn test_map() {
    assert_tokens!(HashMap<u8,u16>, [Tokens::Map]);
    assert_tokens!(HashMap<u8,u16>, [
        Tokens::Map,
        Tokens::U8,
        Tokens::U16,
        Tokens::U8,
        Tokens::U16,
    ], 2);
}

#[test]
fn test_struct() {
    #[derive(Deserialize)]
    struct Foo {
        _a: u8, 
        _b: u16,
    }
    assert_tokens!(Foo, [
        Tokens::Struct("Foo", &["_a", "_b"]),
        Tokens::U8,
        Tokens::U16,
    ]);
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

    assert_tokens!(Foo, [
        Tokens::Enum("Foo", &["A", "B", "C", "D"]),
        Tokens::UnitVariant("A")
    ], 0);

    assert_tokens!(Foo, [
        Tokens::Enum("Foo", &["A", "B", "C", "D"]),
        Tokens::NewTypeVariant("B"),
        Tokens::U8
    ], 1);

    assert_tokens!(Foo, [
        Tokens::Enum("Foo", &["A", "B", "C", "D"]),
        Tokens::TupleVariant("C", 2),
        Tokens::U8,
        Tokens::U16
    ], 2);

    assert_tokens!(Foo, [
        Tokens::Enum("Foo", &["A", "B", "C", "D"]),
        Tokens::StructVariant("D", &["_a"]),
        Tokens::U32,
    ], 3);
}


