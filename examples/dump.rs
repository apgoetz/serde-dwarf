use serde_dwarf;
use std::env;
fn main() {
    let exe = env::current_exe().unwrap();
    serde_dwarf::DebugInfoBuilder::new().parse_path(exe).unwrap();
}
