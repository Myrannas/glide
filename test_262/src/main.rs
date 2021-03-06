mod suite;

extern crate anyhow;
extern crate colored;
extern crate compiler;
extern crate serde;
extern crate serde_yaml;

use crate::suite::{Negative, NegativeType, Phase, Suite, SuiteDetails};
use anyhow::{Context, Error, Result};
use colored::Colorize;
use compiler::{
    compile, parse_input, CompilerOptions, ExecutionError, GlobalThis, JsThread, Module, Object,
    StaticExecutionError,
};
use std::env;
use std::fs::{read_dir, read_to_string};
use std::panic::{catch_unwind, AssertUnwindSafe};
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
    fn load<'a>(&'a self, global_this: GlobalThis<'a>) {
        for module in self.modules.iter() {
            JsThread::new(module.init.clone(), global_this.clone())
                .run()
                .unwrap();
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

enum Outcome {
    Pass,
    Skip,
}

fn run_suite(suite: PathBuf, harness: &ModuleSet) -> Result<Outcome> {
    let Suite { module, details } = Suite::parse(suite)?;

    let harness = AssertUnwindSafe(harness);

    // let compiled = compile("assert.js", module, CompilerOptions::new()).unwrap();
    // let sta_compiled = compile("sta.js", sta_module, CompilerOptions::new()).unwrap();

    // println!("{:#?}", compiled);
    //
    // println!("{:#?}", compiled.load(&global));
    // println!("{:#?}", sta_compiled.load(&global));

    // println!("{:#?}", test_compiled);
    // println!("{:#?}", global);
    //

    if let Some(..) = details.features {
        // panic!("{:?}", details);
        return Ok(Outcome::Skip);
    }

    if let Some(..) = details.flags {
        // panic!("{:?}", details);
        return Ok(Outcome::Skip);
    }

    if let Some(Negative {
        phase: Phase::Parse,
        tpe: NegativeType::SyntaxError,
    }) = details.negative
    {
        return match module {
            Err(StaticExecutionError::SyntaxError(_)) => Ok(Outcome::Pass),
            Ok(..) => Err(Error::msg("Expected a compilation error")),
            Err(..) => Err(Error::msg("Expected a compilation error")),
        };
    }

    let module = AssertUnwindSafe(module);

    match catch_unwind(|| {
        let global = GlobalThis::new();

        harness.load(global.clone());

        let mut thread = JsThread::new(module.0?.init, global.clone());
        thread.set_cost_limit(100000);
        match thread.run() {
            Ok(_) => Ok(()),
            Err(err) => {
                let err: anyhow::Error = err.into();
                Err(err).context(details)
            }
        }
    }) {
        Ok(Ok(..)) => Ok(()),
        Ok(Err(error)) => Err(Error::msg(format!("{:?}", error))),
        Err(error) => Err(Error::msg(format!("{:?}", error))),
    }?;

    Ok(Outcome::Pass)
}

#[derive(PartialOrd, PartialEq)]
enum SuiteResult {
    Success,
    Skip,
    Failure,
}

struct TestResult {
    result: SuiteResult,
    name: String,
    error: Option<String>,
}

fn main() {
    let harness = bootstrap_harness();
    let args: Vec<String> = env::args().collect();
    let pattern = args.get(1);

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
    .filter(|p| {
        if let Some(pattern) = pattern {
            p.clone().to_str().unwrap().contains(pattern)
        } else {
            true
        }
    })
    .collect();

    suites.sort();

    let mut results: Vec<TestResult> = Vec::new();
    for suite in suites {
        let suite_name = suite.to_str().unwrap().to_owned();

        // println!("Running suite {}", suite_name);

        match run_suite(suite.clone(), &harness) {
            Ok(Outcome::Pass) => results.push(TestResult {
                result: SuiteResult::Success,
                name: suite_name,
                error: None,
            }),
            Ok(Outcome::Skip) => results.push(TestResult {
                result: SuiteResult::Skip,
                name: suite_name,
                error: None,
            }),
            Err(err) => results.push(TestResult {
                result: SuiteResult::Failure,
                name: suite_name,
                error: Some(err.to_string()),
            }),
        }
    }

    for TestResult {
        result,
        name,
        error,
    } in results.iter()
    {
        match result {
            SuiteResult::Success => println!("{}", name.green()),
            SuiteResult::Skip => {
                // println!("{}", name.yellow())
            }
            SuiteResult::Failure => {
                println!("{}", name.red());

                if let Some(error) = error {
                    println!("  {}", error.red());
                }
            }
        }
    }

    println!(
        "{} suites succeeded",
        results
            .iter()
            .filter(|f| f.result == SuiteResult::Success)
            .count()
    );
    println!(
        "{} suites failed",
        results
            .iter()
            .filter(|f| f.result == SuiteResult::Failure)
            .count()
    );
    println!(
        "{} suites skipped",
        results
            .iter()
            .filter(|f| f.result == SuiteResult::Skip)
            .count()
    );

    // println!("{:#?}", test_compiled.load(&global));
}
