//! Runs an instance of Domino the Debugger, with no data.

use domino::Data;

fn main() {
    println!("launching on http://localhost:8000");
    domino::launch("127.0.0.1:8000", todo!()).unwrap();
}
