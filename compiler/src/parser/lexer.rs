use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub enum Token<'a> {
    #[regex(r"[0-9]*(\.[0-9]+)?((e|E)[+-]?[1-9][0-9]*)?", float)]
    #[regex(r"0x([0-9a-fA-F]+)", hex)]
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

    #[token("-=")]
    SubAssign,

    #[token("*=")]
    MulAssign,

    #[token("/=")]
    DivAssign,

    #[token("%=")]
    ModAssign,

    #[token("^=")]
    ExpAssign,

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

    #[token("get")]
    Get,

    #[token("set")]
    Set,

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

    #[regex(r"#[$a-zA-Z_][$a-zA-Z0-9_]*")]
    PrivateId(&'a str),

    #[regex(r"'([^'\n\r]|(\\'))*'", string)]
    #[regex(r#""([^"\n\r]|(\\"))*""#, string)]
    String(&'a str),

    #[regex(r"\\/([^\\/]+)\\/", pattern)]
    Pattern(&'a str),

    #[regex(r"`[^`\n]*`", string)]
    TemplateString(&'a str),

    #[regex(r"//[^\u{000A}\u{000D}\u{2028}\u{2029}]*")]
    Comment,

    #[regex(r"/\*(?:[^*]|\*[^/])*\*/", string)]
    BlockComment(&'a str),

    #[error]
    Error,

    #[regex(r"[ \t\f\u{000B}\u{00A0}]+", logos::skip)]
    Whitespace,

    #[regex(r"[\u{000A}\u{000D}\u{2028}\u{2029}]+")]
    NewLine,

    EndOfFile,
}

// Note: callbacks can return `Option` or `Result`
fn float<'b>(lex: &mut Lexer<'b, Token<'b>>) -> Option<f64> {
    let val: &str = lex.slice();
    let result = val.parse();

    Some(result.unwrap_or(f64::NAN))
}

fn hex<'b>(lex: &mut Lexer<'b, Token<'b>>) -> Option<f64> {
    let hex_value: &str = lex.slice();
    let result = u32::from_str_radix(hex_value, 16).map(|v| v as f64);

    Some(result.unwrap_or(f64::NAN))
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

fn pattern<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(slice)
}

impl<'a> Token<'a> {
    pub(crate) fn is_identifier(&self) -> bool {
        self.identifier().is_some()
    }

    pub(crate) fn identifier(&self) -> Option<&'a str> {
        match self {
            Token::Id(value) => Some(value),
            Token::For => Some("for"),
            Token::Get => Some("get"),
            Token::Set => Some("set"),
            _ => None,
        }
    }
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
