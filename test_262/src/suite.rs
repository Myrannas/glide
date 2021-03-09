use anyhow::Context;
use compiler::{compile, parse_input, CompilerError, CompilerOptions, Module};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::fs::read_to_string;
use std::path::PathBuf;

pub struct Suite {
    pub module: Result<Module, CompilerError>,
    pub details: SuiteDetails,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct SuiteDetails {
    info: Option<String>,
    es5id: Option<String>,
    description: String,
    pub negative: Option<Negative>,
    pub features: Option<HashSet<String>>,
    pub flags: Option<HashSet<String>>,
}

impl Display for SuiteDetails {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("suite").field("info", &self.info).finish()?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Negative {
    pub phase: Phase,
    #[serde(rename = "type")]
    pub tpe: NegativeType,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Phase {
    #[serde(rename = "parse")]
    Parse,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum NegativeType {
    SyntaxError,
}

impl Suite {
    pub fn parse(suite: PathBuf) -> anyhow::Result<Suite> {
        let test = read_to_string(suite).unwrap();

        let suite_details_start = test.find("/*---").context("Finding start of details")?;
        let suite_details_end = test.find("---*/").context("Finding end of details")?;

        let details = &test[(suite_details_start + 6)..(suite_details_end - 1)];
        let details: SuiteDetails = serde_yaml::from_str(details)?;

        let module = parse_input(&test)
            .and_then(|module| compile("S8.3_A1_T1.js", module, CompilerOptions::new()));

        Ok(Suite { module, details })
    }
}
