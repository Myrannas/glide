#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Reference<'a> {
    This,
    Id(&'a str),
    Accessor {
        expression: Box<Expression<'a>>,
        accessor: &'a str,
        null_safe: bool,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expression<'a> {
    Float(f64),
    Boolean(bool),
    String(&'a str),
    Null,
    Undefined,
    New(Box<Expression<'a>>),
    NewWithArgs {
        target: Box<Expression<'a>>,
        parameters: Vec<Expression<'a>>,
    },
    Assign {
        assign_to: Reference<'a>,
        expression: Box<Expression<'a>>,
    },
    AddAssign {
        assign_to: Reference<'a>,
        expression: Box<Expression<'a>>,
    },
    Reference(Reference<'a>),
    Call {
        expression: Box<Expression<'a>>,
        parameters: Vec<Expression<'a>>,
    },
    ObjectLiteral {
        attributes: Vec<(&'a str, Expression<'a>)>,
    },
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Sub(Box<Expression<'a>>, Box<Expression<'a>>),
    Mul(Box<Expression<'a>>, Box<Expression<'a>>),
    Div(Box<Expression<'a>>, Box<Expression<'a>>),
    Mod(Box<Expression<'a>>, Box<Expression<'a>>),
    LogicalNot(Box<Expression<'a>>),
    TypeOf(Box<Expression<'a>>),
    Neg(Box<Expression<'a>>),
    LogicalOr(Box<Expression<'a>>, Box<Expression<'a>>),
    LogicalAnd(Box<Expression<'a>>, Box<Expression<'a>>),
    GreaterThan(Box<Expression<'a>>, Box<Expression<'a>>),
    GreaterThanEqual(Box<Expression<'a>>, Box<Expression<'a>>),
    LessThan(Box<Expression<'a>>, Box<Expression<'a>>),
    LessThanEqual(Box<Expression<'a>>, Box<Expression<'a>>),
    NotEqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    EqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    StrictEqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    NotStrictEqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    Function {
        name: Option<&'a str>,
        arguments: Vec<&'a str>,
        statements: Vec<Statement<'a>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ReturnStatement<'a> {
    pub(crate) expression: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct VarStatement<'a> {
    pub(crate) identifier: &'a str,
    pub(crate) expression: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct WhileStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) loop_block: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct IfStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) true_block: Vec<Statement<'a>>,
    pub(crate) false_block: Option<Vec<Statement<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FunctionStatement<'a> {
    pub(crate) identifier: &'a str,
    pub(crate) arguments: Vec<&'a str>,
    pub(crate) statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TryStatement<'a> {
    pub(crate) try_block: Vec<Statement<'a>>,
    pub(crate) catch_binding: Option<&'a str>,
    pub(crate) catch_block: Option<Vec<Statement<'a>>>,
    pub(crate) finally_block: Option<Vec<Statement<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ThrowStatement<'a> {
    pub(crate) expression: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement<'a> {
    Function(FunctionStatement<'a>),
    If(IfStatement<'a>),
    Return(ReturnStatement<'a>),
    While(WhileStatement<'a>),
    Var(VarStatement<'a>),
    Expression(Expression<'a>),
    Try(TryStatement<'a>),
    ThrowStatement(ThrowStatement<'a>),
}

#[derive(Debug, PartialEq)]
pub struct ParsedModule<'a> {
    pub(crate) block: Vec<Statement<'a>>,
}
