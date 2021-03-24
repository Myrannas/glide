use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
enum StringToken {
    #[regex(r"\\u[0-9A-F][0-9A-F][0-9A-F][0-9A-F]", unicode)]
    #[regex(r"\\.", escaped)]
    #[regex(r".", normal)]
    Character(char),

    #[error]
    Error,
}

fn unicode(lex: &mut Lexer<StringToken>) -> Option<char> {
    let slice = &lex.slice()[2..6];

    match u32::from_str_radix(slice, 16) {
        Ok(value) => std::char::from_u32(value),
        Err(_) => None,
    }
}

fn normal(lex: &mut Lexer<StringToken>) -> Option<char> {
    lex.slice().chars().next()
}

fn escaped(lex: &mut Lexer<StringToken>) -> Option<char> {
    match lex.slice() {
        "\\n" => Some('\n'),
        "\\0" => Some('\0'),
        _ => None,
    }
}

pub(crate) fn parse_string(input: &str) -> String {
    let mut lexer: Lexer<StringToken> = StringToken::lexer(input);

    let mut str = String::new();
    while let Some(StringToken::Character(char)) = lexer.next() {
        str.push(char)
    }

    str
}

#[cfg(test)]
mod test {
    use crate::parser::strings::parse_string;

    #[test]
    fn test_parse_normal_string() {
        let input = "String";

        assert_eq!(input, parse_string(input))
    }

    #[test]
    fn test_parse_with_newlines() {
        let input = "With\\nNewline";

        assert_eq!("With\nNewline", parse_string(input))
    }

    #[test]
    fn test_parse_with_unicode() {
        let input = "With \\u2764";

        assert_eq!("With ❤", parse_string(input))
    }
}
