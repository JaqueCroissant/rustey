use std::env;

use rustey::run;

fn main() {
    let mut args = env::args();
    args.next();
    
    match args.next() {
        Some(arg) => run(arg),
        None => return
    };
}
