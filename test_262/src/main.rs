extern crate compiler;

use compiler::value::Object;
use compiler::value::{JsObject, ObjectMethods, RuntimeValue};
use compiler::{compile, parse_input, CompilerOptions, Module};
use std::fs::{read_dir, read_to_string};

fn bootstrap_harness<'a>() -> ModuleSet {
    let sta = read_to_string("./test262/harness/sta.js").unwrap();
    let assert = read_to_string("./test262/harness/assert.js").unwrap();

    let sta_module = parse_input(&sta);
    let module = parse_input(&assert);

    let compiled = compile("assert.js", module, CompilerOptions::new()).unwrap();
    let sta_compiled = compile("sta.js", sta_module, CompilerOptions::new()).unwrap();

    ModuleSet {
        modules: vec![compiled, sta_compiled],
    }
}

struct ModuleSet {
    modules: Vec<Module>,
}

impl ModuleSet {
    fn load<'a, 'b>(&'a self, value: &'b RuntimeValue<'a>) {
        for module in self.modules.iter() {
            module.load(value);
        }
    }
}

fn main() {
    let global = Object::create();

    let test = read_to_string("./test262/test/language/types/boolean/S8.3_A1_T1.js").unwrap();

    let test_module = parse_input(&test);

    // let compiled = compile("assert.js", module, CompilerOptions::new()).unwrap();
    // let sta_compiled = compile("sta.js", sta_module, CompilerOptions::new()).unwrap();

    let test_compiled = compile("S8.3_A1_T1.js", test_module, CompilerOptions::new()).unwrap();

    // println!("{:#?}", compiled);
    //
    // println!("{:#?}", compiled.load(&global));
    // println!("{:#?}", sta_compiled.load(&global));

    let harness = bootstrap_harness();

    harness.load(&global);
    println!("{:#?}", test_compiled);
    println!("{:#?}", global);

    println!("{:#?}", test_compiled.load(&global));
}
