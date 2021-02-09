use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub enum Token<'a> {
    #[regex(r"[0-9]+(\.[0-9]+)?", float)]
    Float(f64),

    #[regex(r"(true|false)", boolean)]
    Boolean(bool),

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Modulus,

    #[token("<")]
    LessThan,

    #[token("=")]
    Assign,

    #[token("<=")]
    LessThanEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanEqual,

    #[token("==")]
    EqualTo,

    #[token("!=")]
    NotEqualTo,

    #[token("===")]
    StrictEqualTo,

    #[token("!==")]
    NotStrictEqualTo,

    #[token("function")]
    Function,

    #[token("return")]
    Return,

    #[token("while")]
    While,

    #[token("var")]
    Var,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("async")]
    Async,

    #[token("await")]
    Await,

    #[token("yield")]
    Yield,

    #[token("new")]
    New,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token(":")]
    Colon,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token(".")]
    Dot,

    #[token("?.")]
    NullSafe,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Id(&'a str),

    #[regex(r"'[^'\n]+'", string)]
    String(&'a str),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,

    EndOfFile,
}

// Note: callbacks can return `Option` or `Result`
fn float<'b>(lex: &mut Lexer<'b, Token<'b>>) -> Option<f64> {
    let val: &str = lex.slice();
    val.parse().ok()
}

// Note: callbacks can return `Option` or `Result`
fn boolean<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<bool> {
    let slice = lex.slice();
    let n = match slice {
        "true" => true,
        "false" => false,
        _ => panic!("Invalid boolean"),
    };
    Some(n)
}

// Note: callbacks can return `Option` or `Result`
fn string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(slice)
}

#[cfg(test)]
mod test {
    use self::super::Token;
    use logos::Logos;

    #[test]
    fn test_float() {
        let mut lex = Token::lexer("1.0");

        assert_eq!(lex.next(), Some(Token::Float(1.0)));
    }
}
