# Introduction
Deserialize [serde](https://serde.rs) data types without implementing
`serde::Deserialize`

`serde_dwarf` provides wrappers to implement `deserialize_any` for
non-self descriptive data types.

This means you can deserialize values without actually having to know
their concrete type.

The `Value` types constructed by `serde_dwarf` can then be inspected
or re-serialized in any arbitrary serde data format, and can be
re-deserialized into the rust data type.

Possible use cases:

+ jq for bincode
+ [serde_transcode](https://docs.rs/serde_transcode) for non-self describing data types
+ deserializer for embedded users of `serde`

# How It Works

`serde_dwarf` works by parsing the DWARF debuginfo that is generated
for any `serde` using code. This provides enough information to
reconstruct the `#[derive(Serialize)]` impls, assuming that the code
isn't using too many of the serde attribute decorators. Once
`serde_dwarf` knows about the type definition, you can now deserialize
it into a generic `Value` enum.

Note that you don't actually need to tell serde_dwarf about the
`Serialize` impls at compile time, all of the parsing is done
dynamically. This allows for the creation of `serde` inspection tools
that are crate-agnostic.

# Caveats and Soundness

`serde_dwarf` works be reconstructing the `Serialize` impl that
`serde` would have created for a data type. This means that it cannot
work on data types that define their own custom deserialization code,
for example [uuid](https://docs.rs/uuid). In addition, if your code
makes use of `serde` optional attributes to customize the
serialization, for
example [skip](https://serde.rs/field-attrs.html#skip), the
deserialization will not be correct.

This also means that `serde_dwarf` contains a potentially buggy
reimplementation of the serialize and deserialize derive macros, that
could potentially get out of date as serde is updated. We try to stay
on top of this by testing that `serde_dwarf` drives the
Deserialization Visitors in exactly the same way as `serde` itself,
but there is some duplication of effort here.

This means that if you specify the wrong data type for the conversion,
or if the `Deserialize` impl has a custom implementation, you will
get garbage data, and `serde_dwarf` may or may not report an error. 

Taken together, this means that `serde_dwarf` really isn't suited for
use in a production environment, but instead for tools for inspecting
and manipulating `serde` serialized data. Think of it more like gdb's
rust parser.

# Example

```rust
use serde_dwarf::DebugInfoBuilder; 
use serde::de::DeserializeSeed; 
use bincode::Options;

// an instance we want to serialize 
let target = ("abc", 1, 2, 3);

// serialize it to bytes using bincode 
let encoded: Vec<u8> = bincode::serialize(&target).unwrap();

// read out the types defined in the debug info of this executable 
let di = DebugInfoBuilder::new().parse_path(std::env::current_exe().unwrap()).unwrap();
let typ = di.typ("(&str, i32, i32, i32)").unwrap();

// build a bincode deserializer 
let opts = bincode::DefaultOptions::new().with_fixint_encoding(); 
let mut deserializer = bincode::Deserializer::from_slice(&encoded, opts);

// we can now deserialize the bytes into a Value struct.
// note that we aren't actually using the type of target above
println!("{:?}",typ.deserialize(&mut deserializer).unwrap());
```
