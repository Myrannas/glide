extern crate anyhow;
extern crate colored;
extern crate compiler;

use anyhow::{Error, Result};
use colored::Colorize;
use compiler::value::Object;
use compiler::value::{JsObject, ObjectMethods, RuntimeValue};
use compiler::{compile, parse_input, CompilerOptions, Module};
use std::fs::{read_dir, read_to_string};
use std::panic::catch_unwind;
use std::path::PathBuf;

fn bootstrap_harness<'a>() -> ModuleSet {
    let sta = read_to_string("./test262/harness/sta.js").unwrap();
    let assert = read_to_string("./test262/harness/assert.js").unwrap();

    let sta_module = parse_input(&sta).unwrap();
    let module = parse_input(&assert).unwrap();

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

fn all_suites(path: PathBuf) -> std::io::Result<Vec<PathBuf>> {
    let mut values = Vec::new();

    for path in read_dir(path)? {
        let result = path?;

        if result.file_type()?.is_dir() {
            values.append(&mut all_suites(result.path())?);
        } else {
            values.push(result.path());
        }
    }

    Ok(values)
}

fn run_suite(suite: PathBuf, harness: &ModuleSet) -> Result<()> {
    let test = read_to_string(suite).unwrap();

    let test_module = parse_input(&test)?;

    // let compiled = compile("assert.js", module, CompilerOptions::new()).unwrap();
    // let sta_compiled = compile("sta.js", sta_module, CompilerOptions::new()).unwrap();

    // println!("{:#?}", compiled);
    //
    // println!("{:#?}", compiled.load(&global));
    // println!("{:#?}", sta_compiled.load(&global));

    // println!("{:#?}", test_compiled);
    // println!("{:#?}", global);
    //

    match catch_unwind(|| {
        let global = Object::create();

        harness.load(&global);

        let test_compiled = compile("S8.3_A1_T1.js", test_module, CompilerOptions::new())
            .expect("Compiling module");

        test_compiled.load(&global).expect("Loading module");
    }) {
        Ok(..) => Ok(()),
        Err(..) => Err(Error::msg("Failed")),
    }?;

    Ok(())
}

#[derive(PartialOrd, PartialEq)]
enum SuiteResult {
    Success,
    Failure,
}

fn main() {
    let harness = bootstrap_harness();

    let mut suites: Vec<PathBuf> = vec![
        PathBuf::from("./test262/test/language"),
        PathBuf::from("./test262/test/built-ins"),
        PathBuf::from("./test262/test/annexB"),
        PathBuf::from("./test262/test/intl402"),
        // PathBuf::from("./test262/implementation-contributed"),
        // PathBuf::from("./test262/test/statements"),
    ]
    .into_iter()
    .flat_map(|f| all_suites(f).unwrap())
    .collect();

    suites.sort();

    let mut results = Vec::new();
    for suite in suites {
        let suite_name = suite.to_str().unwrap().to_owned();
        match run_suite(suite.clone(), &harness) {
            Ok(_) => results.push((SuiteResult::Success, suite_name)),
            Err(_) => results.push((SuiteResult::Failure, suite_name)),
        }
    }

    for (result, suite) in results.iter() {
        match result {
            SuiteResult::Success => println!("{}", suite.green()),
            SuiteResult::Failure => println!("{}", suite.red()),
        }
    }

    println!(
        "{} suites succeeded",
        results
            .iter()
            .filter(|f| f.0 == SuiteResult::Success)
            .count()
    );
    println!(
        "{} suites failed",
        results
            .iter()
            .filter(|f| f.0 == SuiteResult::Failure)
            .count()
    );

    // println!("{:#?}", test_compiled.load(&global));
}
