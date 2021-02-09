#[cfg(test)]
mod test {
    use crate::parser::parse_module;
    use std::fs::read_to_string;

    #[test]
    fn test_assert() {
        let assert = read_to_string("./test262/harness/assert.js").unwrap();

        parse_module(&assert);
    }
}
