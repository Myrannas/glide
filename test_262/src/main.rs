mod suite;
mod tree;

extern crate anyhow;
extern crate clap;
extern crate colored;
extern crate glide_compiler;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

use std::cell::RefCell;

thread_local! {
    static REALM: RefCell<Realm<'static>> = RefCell::new(bootstrap_harness());
}
use crate::suite::{Negative, NegativeType, Phase, Suite};
use crate::tree::Tree;
use anyhow::{Context, Error, Result};
use clap::{App, Arg};
use colored::Colorize;
use glide_compiler::{compile, parse_input, CompilerError, CompilerOptions};
use glide_runtime::{ExecutionError, JsFunction, JsThread, Realm, Unwrap, ValueType};
use rayon::prelude::*;
use rayon::ThreadPoolBuilder;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fs::{read_dir, read_to_string, write};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::PathBuf;
use std::time::Duration;

fn bootstrap_harness<'a>() -> Realm<'a> {
    let mut realm = Realm::new();

    for file in &[
        "./test262/harness/sta.js",
        "./test262/harness/assert.js",
        "./test262/harness/propertyHelper.js",
    ] {
        let sta = read_to_string(file).unwrap();
        let sta_module = parse_input(&sta).unwrap();

        // println!("{:#?}", sta_module);

        let compiled = compile("assert.js", sta_module, CompilerOptions::new()).unwrap();

        let module = JsFunction::load(compiled.init, &mut realm);

        let mut thread = JsThread::new(module.clone(), realm.clone());

        let result = thread.run();

        match result {
            Ok(_) => {
                realm = thread.finalize();
            }
            Err(other) => {
                panic!("{:?}", thread.debug(&other))
            }
        }
    }

    realm
}

struct ModuleSet<'a> {
    realm: Realm<'a>,
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
    Pass(Duration),
    Skip,
}

fn run_suite(suite: PathBuf, cost_limit: usize) -> Result<Outcome> {
    let Suite { module, details } = Suite::parse(suite)?;

    // let harness = AssertUnwindSafe(harness);

    // let compiled = compile("assert.js", module, CompilerOptions::new()).unwrap();
    // let sta_compiled = compile("sta.js", sta_module, CompilerOptions::new()).unwrap();

    // println!("{:#?}", compiled);
    //
    // println!("{:#?}", compiled.load(&global));
    // println!("{:#?}", sta_compiled.load(&global));

    // println!("{:#?}", test_compiled);
    // println!("{:#?}", global);
    //

    let supported_features = vec![].into_iter().collect::<HashSet<_>>();

    if let Some(features) = details.features.clone() {
        let unsupported_features = features.difference(&supported_features);

        if unsupported_features.count() > 0 {
            return Ok(Outcome::Skip);
        }
    }

    if let Some(flags) = &details.flags {
        // panic!("{:?}", details);

        if flags.len() > 1 || !flags.contains("generated") {
            return Ok(Outcome::Skip);
        }
    }

    if let Some(Negative {
        phase: Phase::Parse,
        tpe: NegativeType::SyntaxError,
    }) = details.negative
    {
        return match module {
            Err(CompilerError::SyntaxError(_)) => Ok(Outcome::Pass(Duration::from_secs(0))),
            Ok(..) => Err(Error::msg("Expected a compilation error")),
            Err(..) => Err(Error::msg("Expected a compilation error")),
        };
    }

    let module = AssertUnwindSafe(module);

    let time = match catch_unwind(|| {
        let mut global = REALM.with(|f| f.borrow().clone());
        // let mut global = harness.realm.clone();

        let start = std::time::Instant::now();

        let function = JsFunction::load(module.0?.init, &mut global);

        let mut thread = JsThread::new(function, global);
        thread.set_cost_limit(cost_limit);

        let result = thread.run();

        if let Some(Negative {
            phase: Phase::Runtime,
            tpe: NegativeType::Test262Error,
        }) = details.negative
        {
            match result {
                Ok(_) => Err(Error::msg("Expected a Test262Error error")),
                Err(ExecutionError::Thrown(value, ..)) => {
                    let obj = value
                        .to_object(&mut thread)
                        .unwrap_value(thread.get_realm());
                    let to_string = thread.get_realm_mut().intern_string("toString");
                    let to_string = obj
                        .get_value(&mut thread, to_string)
                        .unwrap_value(thread.get_realm());

                    if let ValueType::Object(fn_object) = to_string.get_type() {
                        let result = fn_object
                            .call(&mut thread, &[])
                            .unwrap_value(thread.get_realm())
                            .to_string(&mut thread)
                            .unwrap_value(thread.get_realm());

                        if thread
                            .get_realm()
                            .get_string(result)
                            .starts_with("Test262Error:")
                        {
                            Ok(std::time::Instant::now() - start)
                        } else {
                            Err(Error::msg("Expected a Test262Error error"))
                        }
                    } else {
                        Err(Error::msg("Expected a Test262Error error"))
                    }
                }
                Err(err) => {
                    let err = err.render(&mut thread);
                    Err(err).context(details)
                }
            }
        } else {
            match result {
                Ok(_) => Ok(std::time::Instant::now() - start),
                Err(err) => {
                    let err = err.render(&mut thread);
                    Err(err).context(details)
                }
            }
        }
    }) {
        Ok(Ok(duration)) => Ok(duration),
        Ok(Err(error)) => Err(Error::msg(format!("{:?}", error))),
        Err(error) => Err(Error::msg(format!("{:?}", error))),
    }?;

    Ok(Outcome::Pass(time))
}

#[derive(PartialOrd, PartialEq, Serialize, Deserialize, Clone, Debug)]
enum SuiteResult {
    Success,
    Skip,
    Failure,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
struct TestResult {
    result: SuiteResult,
    name: String,

    #[serde(default)]
    runtime: f64,

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
            Arg::with_name("profile")
                .help("Profile")
                .short("p")
                .long("profile"),
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

    ThreadPoolBuilder::new()
        .num_threads(8)
        .stack_size(4194304)
        .panic_handler(|err| panic!("Ohnoes"))
        .build_global()
        .unwrap();

    let mut suites: Vec<PathBuf> = vec![
        // PathBuf::from("./test262/test"),
        // PathBuf::from("./test262/test/built-ins"),
        // PathBuf::from("./test262/test/annexB"),
        // PathBuf::from("./test262/test/intl402"),
        // PathBuf::from("./test262/implementation-contributed"),
        PathBuf::from("./test262/test/language"),
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

    let success_time = Duration::from_micros(0);
    let is_profiling = matches.is_present("profile");
    let runs = if is_profiling { 500 } else { 1 };
    let results: Vec<TestResult> = suites
        .par_iter()
        .map(|suite| {
            let suite_name = suite.to_str().unwrap().to_owned();

            print!("Running suite {:80.80}", suite_name);
            //
            let mut runtime = Duration::from_micros(0);
            let mut passed = true;
            match run_suite(
                suite.clone(),
                matches.value_of("limit").unwrap().parse().unwrap(),
            ) {
                Ok(Outcome::Pass(time)) => {
                    if time != Duration::from_micros(0) {
                        runtime += time;

                        // if !is_profiling {
                        println!("{}", "pass".green());
                        // } else if run % 100 == 99 {
                        //     println!(
                        //         "{}: {} in {:.1}us",
                        //         run,
                        //         "pass".green(),
                        //         (runtime.as_secs_f64() * 1_000_000.0) / (run as f64)
                        //     );
                        // }
                    } else {
                        println!("{} (expected Syntax Error)", "pass".green());
                        // break;
                    }

                    TestResult {
                        result: SuiteResult::Success,
                        name: suite_name.clone(),
                        error: None,
                        runtime: time.as_secs_f64(),
                    }
                }
                Ok(Outcome::Skip) => {
                    println!("{}", "skip".yellow());
                    TestResult {
                        result: SuiteResult::Skip,
                        name: suite_name.clone(),
                        error: None,
                        runtime: 0.0,
                    }
                }
                Err(err) => {
                    println!("{}", "fail".red());
                    TestResult {
                        result: SuiteResult::Failure,
                        name: suite_name.clone(),
                        error: Some(err.to_string()),
                        runtime: 0.0,
                    }
                }
            }

            // if passed {
            //     results.push();
            //
            //     success_time += runtime / (runs as u32);
            // }
        })
        .collect();

    let compare = matches.is_present("compare");
    let commit = matches.is_present("commit");
    let input = read_to_string("./target/results.json").unwrap_or_else(|_| "{}".to_owned());

    let mut previous: Tree<String, TestResult> = serde_json::from_str(&input).unwrap_or_default();

    let differences: Vec<(TestResult, TestResult)> = results
        .into_iter()
        .filter_map(|result| {
            let result_to_store = TestResult {
                name: result.name.clone(),
                runtime: if is_profiling {
                    result.runtime
                } else {
                    previous
                        .get(result.name.split('/'))
                        .map(|t| t.runtime)
                        .unwrap_or_default()
                },
                error: None,
                result: result.result.clone(),
            };

            match previous.insert(result.name.split('/'), result_to_store) {
                None => None,
                Some(previous) => {
                    let difference = (previous.runtime - result.runtime).abs();
                    let difference_percent = (difference / previous.runtime) * 100.0;

                    if !compare
                        || previous.result != result.result
                        || (is_profiling && difference_percent > 10.0)
                    {
                        Some((result.clone(), previous))
                    } else {
                        None
                    }
                }
            }
        })
        .collect();

    previous.reduce(
        |_depth, _key, leaf| {
            if leaf.result == SuiteResult::Success {
                (1, 1)
            } else {
                (0, 1)
            }
        },
        |depth, key, branch| {
            let mut total = 0;
            let mut total_success = 0;

            for (success, count) in branch {
                total += count;
                total_success += success;
            }

            println!(
                "{}{:<30}{} {:>5}/{:>5}    {:3.3}%",
                "  ".repeat(depth),
                key,
                "  ".repeat(15 - depth),
                total_success,
                total,
                (total_success as f64) / (total as f64) * 100.0
            );

            (total_success, total)
        },
    );

    if commit {
        write(
            "./target/results.json",
            serde_json::to_string_pretty(&previous).unwrap(),
        )
        .unwrap();
    }

    for (difference, previous) in differences.iter() {
        match difference.result {
            SuiteResult::Success => {
                let time_difference = if previous.runtime != 0.0 {
                    let time_difference = difference.runtime - previous.runtime;

                    if time_difference > 0.0 {
                        format!(
                            "{:.3}+{:.3}us",
                            previous.runtime * 1_000_000.0,
                            time_difference * 1_000_000.0
                        )
                        .red()
                    } else {
                        format!(
                            "{:.3}{:.3}us",
                            previous.runtime * 1_000_000.0,
                            time_difference * 1_000_000.0
                        )
                        .green()
                    }
                } else {
                    "".to_owned().green()
                };

                println!("{} {}", difference.name.green(), time_difference)
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
        .filter(|(current, previous)| !compare || current.result != previous.result)
        .filter(|f| f.0.result == SuiteResult::Success)
        .count();

    let skip_count = differences
        .iter()
        .filter(|f| f.0.result == SuiteResult::Skip)
        .count();

    let failure_count = differences
        .iter()
        .filter(|f| f.0.result == SuiteResult::Failure)
        .count();

    // let total_success_count = previous
    //     .values()
    //     .filter(|f| f.result == SuiteResult::Success)
    //     .count();

    // let total_count = previous.len();

    // let percent_passing = (total_success_count as f64 / total_count as f64) * 100.0;

    // println!(
    //     "{} / {} passing ({:3.1} %)",
    //     total_success_count, total_count, percent_passing
    // );

    if compare {
        println!("{} new suites succeeded", success_count);
        println!("{} new suites failed", failure_count);
        println!("{} new suites skipped", skip_count);
        println!(
            "{:.3}ms successful test time",
            success_time.as_secs_f64() * 1000.0
        );

        if !differences.is_empty() {
            println!("Remember to update the ratchet with changes");
            std::process::exit(1);
        }
    } else {
        println!("{} suites succeeded", success_count);
        println!("{} suites failed", failure_count);
        println!("{} suites skipped", skip_count);
        println!(
            "{:.3}ms successful test time",
            success_time.as_secs_f64() * 1000.0
        );
    }
}
