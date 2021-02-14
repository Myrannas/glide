#[cfg(test)]
mod test {
    use crate::compiler::compile;
    use crate::parser::parse_input;
    use crate::value::JsObject;
    use std::fs::{read_dir, read_to_string};

    #[test]
    fn test_assert() {
        let global = JsObject::create();

        let sta = read_to_string("./test262/harness/sta.js").unwrap();
        let assert = read_to_string("./test262/harness/assert.js").unwrap();

        let sta_module = parse_input(&sta);
        let module = parse_input(&assert);

        let compiled = compile("assert.js", module).unwrap();
        let sta_compiled = compile("sta.js", sta_module).unwrap();

        println!("{:#?}", compiled);

        println!("{:#?}", compiled.load(&global));
        println!("{:#?}", sta_compiled.load(&global));
        println!("{:#?}", global);
    }
}
