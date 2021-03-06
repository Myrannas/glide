mod suite;

extern crate anyhow;
extern crate clap;
extern crate colored;
extern crate compiler;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

use crate::suite::{Negative, NegativeType, Phase, Suite};
use anyhow::{Context, Error, Result};
use clap::{App, Arg};
use colored::Colorize;
use compiler::{
    compile, parse_input, CompilerOptions, GlobalThis, JsThread, Module, StaticExecutionError,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs::{read_dir, read_to_string, write};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::PathBuf;

fn bootstrap_harness() -> ModuleSet {
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

fn run_suite(suite: PathBuf, harness: &ModuleSet, cost_limit: usize) -> Result<Outcome> {
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
        thread.set_cost_limit(cost_limit);
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

#[derive(PartialOrd, PartialEq, Serialize, Deserialize, Clone)]
enum SuiteResult {
    Success,
    Skip,
    Failure,
}

#[derive(Clone, Serialize, Deserialize)]
struct TestResult {
    result: SuiteResult,
    name: String,

    #[serde(skip_serializing)]
    error: Option<String>,
}

fn main() {
    let matches = App::new("test_262")
        .arg(
            Arg::with_name("pattern")
                .short("p")
                .long("pattern")
                .takes_value(true)
                .index(1),
        )
        .arg(
            Arg::with_name("compare")
                .help("Compare with results in provided results file. Return non-zero exit code on additional failures")
                .short("c")
                .long("compare"),
        )
        .arg(
            Arg::with_name("commit")
                .help("Write the test results")
                .short("x")
                .long("commit"),
        )
        .arg(
            Arg::with_name("limit")
                .help("Write the test results")
                .short("l")
                .long("limit")
                .default_value("100000"),
        )
        .get_matches();

    let pattern = matches.value_of("pattern");

    let harness = bootstrap_harness();

    let mut suites: Vec<PathBuf> = vec![
        PathBuf::from("./test262/test"),
        // PathBuf::from("./test262/test/built-ins"),
        // PathBuf::from("./test262/test/annexB"),
        // PathBuf::from("./test262/test/intl402"),
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

        println!("Running suite {:80.80}", suite_name);

        match run_suite(
            suite.clone(),
            &harness,
            matches.value_of("limit").unwrap().parse().unwrap(),
        ) {
            Ok(Outcome::Pass) => {
                println!("{}", "pass".green());
                results.push(TestResult {
                    result: SuiteResult::Success,
                    name: suite_name,
                    error: None,
                })
            }
            Ok(Outcome::Skip) => {
                println!("{}", "skip".yellow());
                results.push(TestResult {
                    result: SuiteResult::Skip,
                    name: suite_name,
                    error: None,
                })
            }
            Err(err) => {
                println!("{}", "fail".red());
                results.push(TestResult {
                    result: SuiteResult::Failure,
                    name: suite_name,
                    error: Some(err.to_string()),
                })
            }
        }
    }

    let compare = matches.is_present("compare");
    let commit = matches.is_present("commit");
    let input = read_to_string("./target/results.json").unwrap_or_else(|_| "{}".to_owned());

    let mut previous: BTreeMap<_, _> = serde_json::from_str(&input).unwrap_or_default();

    let differences: Vec<TestResult> = results
        .into_iter()
        .filter(
            |result| match previous.insert(result.name.clone(), result.clone()) {
                None => false,
                Some(previous) => !compare || previous.result != result.result,
            },
        )
        .collect();

    if commit {
        write(
            "./target/results.json",
            serde_json::to_string_pretty(&previous).unwrap(),
        )
        .unwrap();
    }

    for difference in differences.iter() {
        match difference.result {
            SuiteResult::Success => {
                println!("{}", difference.name.green())
            }
            SuiteResult::Failure => {
                println!("{}", difference.name.red());

                if let Some(error) = &difference.error {
                    println!("{}", error.red());
                }
            }
            _ => (),
        }
    }

    let success_count = differences
        .iter()
        .filter(|f| f.result == SuiteResult::Success)
        .count();

    let skip_count = differences
        .iter()
        .filter(|f| f.result == SuiteResult::Skip)
        .count();

    let failure_count = differences
        .iter()
        .filter(|f| f.result == SuiteResult::Failure)
        .count();

    let total_success_count = previous
        .values()
        .filter(|f| f.result == SuiteResult::Success)
        .count();

    let total_count = previous.len();

    let percent_passing = (total_success_count as f64 / total_count as f64) * 100.0;

    println!(
        "{} / {} passing ({:3.1} %)",
        total_success_count, total_count, percent_passing
    );

    if compare {
        println!("{} new suites succeeded", success_count);
        println!("{} new suites failed", failure_count);
        println!("{} new suites skipped", skip_count);

        if !differences.is_empty() {
            println!("Remember to update the ratchet with changes");
            std::process::exit(1);
        }
    } else {
        println!("{} suites succeeded", success_count);
        println!("{} suites failed", failure_count);
        println!("{} suites skipped", skip_count);
    }
}
