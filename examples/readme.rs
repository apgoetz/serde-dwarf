// the example that is used in the README

fn main() {
    use bincode::Options;
    use serde::de::DeserializeSeed;
    use serde_dwarf::DebugInfoBuilder;

    // an instance we want to serialize
    let target = ("abc", 1, 2, 3);

    // serialize it to bytes using bincode
    let encoded: Vec<u8> = bincode::serialize(&target).unwrap();

    // read out the types defined in the debug info of this executable
    let di = DebugInfoBuilder::new()
        .parse_path(std::env::current_exe().unwrap())
        .unwrap();
    let typ = di.typ("(&str, i32, i32, i32)").unwrap();

    // build a bincode deserializer
    let opts = bincode::DefaultOptions::new().with_fixint_encoding();
    let mut deserializer = bincode::Deserializer::from_slice(&encoded, opts);

    // we can now deserialize the bytes into a Value struct
    // note that we aren't actually using the type of target above
    println!("{:?}", typ.deserialize(&mut deserializer).unwrap());
}
