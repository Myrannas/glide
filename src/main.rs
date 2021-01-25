mod compiler;
mod ops;
mod parser;
mod value;
mod vm;

use crate::compiler::compile;
use crate::parser::parse_module;

extern crate anyhow;
extern crate nom;

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
    match compile(
        "main",
        parse_module(
            "
function test(n) {
    var a = 1;
    function testa() {
        return a;
    };

    return testa();
};

test();
",
        ),
    ) {
        Ok(module) => {
            let r = module.load();

            println!("Result {:?}", r);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    }
}
