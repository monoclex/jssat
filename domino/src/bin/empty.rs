//! Runs an instance of Domino the Debugger, with no data.

fn main() {
    println!("launching on http://localhost:8080");
    domino::launch("127.0.0.1:8080").unwrap();
}
