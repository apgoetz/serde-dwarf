use serde_dwarf;
use std::env;
use serde::Serialize;
use serde_json;
use std::io;

#[derive(Serialize, Copy, Clone)]
struct Foo<'a> {
    bar: u32,
    baz: &'a str,
}

fn main() {
    let foo = Foo{bar: 1, baz: "test"};
    let mut serializer = serde_json::Serializer::new(io::stdout());
    let my_array = [1,2,3];
    let my_slice = &my_array[0..1];
    let my_foo_array = [Foo{bar: 1, baz: "test"} ; 5];
    let my_foo_slice = &my_foo_array[0..1];
    foo.serialize(&mut serializer).unwrap();
    my_array.serialize(&mut serializer).unwrap();
    my_slice.serialize(&mut serializer).unwrap();
    my_foo_array.serialize(&mut serializer).unwrap();
    my_foo_slice.serialize(&mut serializer).unwrap();
    let j = serde_json::to_string(&foo).unwrap();
    println!("{}", j);
    let my_exe = env::current_exe().unwrap();
    serde_dwarf::DebugInfoBuilder::new().parse_path(my_exe).unwrap();
}
