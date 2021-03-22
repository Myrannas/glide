use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub enum Token<'a> {
    #[regex(r"[-]?[0-9]+(\.[0-9]+)?(e[+-]?[1-9][0-9]*)?", float)]
    Float(f64),

    #[regex(r"(true|false)", boolean)]
    Boolean(bool),

    #[token("+")]
    Add,

    #[token("++")]
    Inc,

    #[token("--")]
    Dec,

    #[token("-")]
    Sub,

    #[token("*")]
    Multiply,

    #[token("**")]
    Exponential,

    #[token("/")]
    Divide,

    #[token("%")]
    Modulus,

    #[token("<")]
    LessThan,

    #[token("=")]
    Assign,

    #[token("!")]
    LogicalNot,

    #[token("+=")]
    AddAssign,

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

    #[token(">>>")]
    UnsignedRightShift,

    #[token("<<")]
    LeftShift,

    #[token(">>")]
    RightShift,

    #[token("function")]
    Function,

    #[token("typeof")]
    TypeOf,

    #[token("instanceof")]
    InstanceOf,

    #[token("null")]
    Null,

    #[token("void")]
    Void,

    #[token("return")]
    Return,

    #[token("while")]
    While,

    #[token("var")]
    Var,

    #[token("let")]
    Let,

    #[token("for")]
    For,

    #[token("const")]
    Const,

    #[token("async")]
    Async,

    #[token("try")]
    Try,

    #[token("break")]
    Break,

    #[token("delete")]
    Delete,

    #[token("continue")]
    Continue,

    #[token("throw")]
    Throw,

    #[token("in")]
    In,

    #[token("catch")]
    Catch,

    #[token("finally")]
    Finally,

    #[token("class")]
    Class,

    #[token("extends")]
    Extends,

    #[token("await")]
    Await,

    #[token("yield")]
    Yield,

    #[token("new")]
    New,

    #[token("this")]
    This,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("static")]
    Static,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[token(":")]
    Colon,

    #[token("?")]
    QuestionMark,

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

    #[token("||")]
    LogicalOr,

    #[token("&&")]
    LogicalAnd,

    #[regex(r"[$a-zA-Z_][$a-zA-Z0-9_]*")]
    Id(&'a str),

    #[regex(r"'([^'\n\r]|(\\'))*'", string)]
    #[regex(r#""([^"\n\r]|(\\"))*""#, string)]
    String(&'a str),

    #[regex(r"`[^`\n]*`", string)]
    TemplateString(&'a str),

    #[regex(r"//[^\u{000A}\u{000D}\u{2028}\u{2029}]*")]
    Comment,

    #[regex(r"/\*(?:[^*]|\*[^/])*\*/", string)]
    BlockComment(&'a str),

    #[error]
    Error,

    #[regex(r"[ \t\f]+", logos::skip)]
    Whitespace,

    #[regex(r"[\u{000A}\u{000D}\u{2028}\u{2029}]+")]
    NewLine,

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
