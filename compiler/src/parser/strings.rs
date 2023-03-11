use crate::parser::hand_parser::Error;
use crate::parser::hand_parser::Error::InvalidUnicodeEscape;
use itertools::Itertools;
use logos::{Lexer, Logos};
use std::iter::Peekable;
use std::str::Chars;

// #[derive(Logos, Debug, PartialEq, Copy, Clone)]
// enum StringToken {
//     #[regex(r"\\u[0-9A-F][0-9A-F][0-9A-F][0-9A-F]", unicode)]
//     #[regex(r"\\.", escaped)]
//     #[regex(r".", normal)]
//     Character(char),
//
//     #[error]
//     Error,
// }
//
// fn unicode(lex: &mut Lexer<StringToken>) -> Option<char> {
//     println!("{}", lex.slice());
//
//     let slice = &lex.slice()[2..6];
//
//     match u32::from_str_radix(slice, 16) {
//         Ok(value) => std::char::from_u32(value),
//         Err(_) => None,
//     }
// }
//
// fn normal(lex: &mut Lexer<StringToken>) -> Option<char> {
//     println!("{}", lex.slice());
//     lex.slice().chars().next()
// }
//
// fn escaped(lex: &mut Lexer<StringToken>) -> Option<char> {
//     println!("{}", lex.slice());
//
//     match lex.slice() {
//         "\\n" => Some('\n'),
//         "\\f" => Some('\u{000C}'),
//         "\\v" => Some('\u{000B}'),
//         "\\0" => Some('\0'),
//         "\\'" => Some('\''),
//         "\\\"" => Some('"'),
//         "\\/" => Some('/'),
//         "\\\\" => Some('\\'),
//         _ => None,
//     }
// }

const ESCAPE: char = '\\';
const UNICODE: char = 'u';

fn parse_unicode<'a>(parser: &mut Peekable<Chars>) -> Result<char, Error<'a>> {
    let results: String = parser.take(4).collect();

    u32::from_str_radix(&results, 16)
        .map_err(|_err| InvalidUnicodeEscape)
        .and_then(|v| char::from_u32(v).ok_or(InvalidUnicodeEscape))
}

pub(crate) fn parse_string(input: &str) -> Result<String, Error> {
    // let mut lexer: Lexer<StringToken> = StringToken::lexer(input);
    //
    // let mut str = String::new();
    // while let Some(StringToken::Character(char)) = lexer.next() {
    //     str.push(char)
    // }

    let mut output = String::with_capacity(input.len());
    let mut iter = input.chars().peekable();

    while let Some(next) = iter.next() {
        output.push(match next {
            ESCAPE => {
                let next = iter.next();

                match next {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('v') => '\u{000B}',
                    Some('b') => '\u{0008}',
                    Some('f') => '\u{000C}',
                    Some('0') => '\u{0000}',
                    Some(UNICODE) => parse_unicode(&mut iter)?,
                    Some(other) => other,
                    _ => '\\',
                }
            }
            other => other,
        })
    }

    Ok(output)
}

#[cfg(test)]
mod test {
    use crate::parser::strings::parse_string;

    #[test]
    fn test_parse_normal_string() {
        let input = "String";

        assert_eq!(input, parse_string(input).unwrap())
    }

    #[test]
    fn test_parse_with_newlines() {
        let input = "With\\nNewline";

        assert_eq!("With\nNewline", parse_string(input).unwrap())
    }

    #[test]
    fn test_parse_with_unicode() {
        let input = "With \\u2764";

        assert_eq!("With ❤", parse_string(input).unwrap())
    }

    #[test]
    fn test_parse_with_invalid_sequence() {
        let input = "With \u{0004} test";

        assert_eq!(parse_string(input).is_err(), true)
    }
}
