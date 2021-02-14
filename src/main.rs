mod compiler;
mod ops;
mod parser;
mod tc262;
mod value;
mod vm;

use crate::compiler::compile;
use crate::parser::parse_input;
use crate::value::JsObject;

extern crate anyhow;
extern crate log;
extern crate pretty_env_logger;

// macro_rules! option {
//     () => {
//         None
//     };
//     ($e:expr) => {
//         Some($e)
//     };
// }
//
// macro_rules! block {
//     ($($i:ident $( $v: expr )? ),*) => (vec![ $(Instruction { instr: $i, constant: option!($($v)?) }),* ]);
//     // ($i: expr, $v: expr ) => (Instruction { instr: $i, constant: Some($v) });
// }

fn main() {
    pretty_env_logger::init();

    let start = std::time::Instant::now();

    let parsed_module = parse_input(
        "
// Test program
var console = {
    log: 'log'
};
var i = 0;
var test = {
    test: {
        abc: 123
    }
};
console.log(test.test);
",
    );

    println!("{:#?}", parsed_module);

    match compile("main", parsed_module) {
        Ok(module) => {
            println!("{:#?}", module);

            let global_this = JsObject::create();
            let r = module.load(&global_this);

            println!(
                "Execution time {} ms",
                (std::time::Instant::now() - start).as_secs_f64() * 1000.0
            );

            println!("Result {:?}", r);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    }
}
