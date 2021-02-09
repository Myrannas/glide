#[derive(Debug, PartialEq)]
pub(crate) enum Reference<'a> {
    Id(&'a str),
    Accessor {
        expression: Box<Expression<'a>>,
        accessor: &'a str,
        null_safe: bool,
    },
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expression<'a> {
    Float(f64),
    Boolean(bool),
    String(&'a str),
    Assign {
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
    GreaterThan(Box<Expression<'a>>, Box<Expression<'a>>),
    GreaterThanEqual(Box<Expression<'a>>, Box<Expression<'a>>),
    LessThan(Box<Expression<'a>>, Box<Expression<'a>>),
    LessThanEqual(Box<Expression<'a>>, Box<Expression<'a>>),
    NotEqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    EqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    StrictEqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
    NotStrictEqualTo(Box<Expression<'a>>, Box<Expression<'a>>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Return<'a> {
    pub(crate) expression: Expression<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct VarStatement<'a> {
    pub(crate) identifier: &'a str,
    pub(crate) expression: Expression<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct WhileStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) loop_block: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct IfStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) true_block: Vec<Statement<'a>>,
    pub(crate) false_block: Option<Vec<Statement<'a>>>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct FunctionStatement<'a> {
    pub(crate) identifier: &'a str,
    pub(crate) arguments: Vec<&'a str>,
    pub(crate) statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Statement<'a> {
    Function(FunctionStatement<'a>),
    IfStatement(IfStatement<'a>),
    Return(Return<'a>),
    While(WhileStatement<'a>),
    Var(VarStatement<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct ParsedModule<'a> {
    pub(crate) block: Vec<Statement<'a>>,
}
