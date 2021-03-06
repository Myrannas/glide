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
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqualTo,
    EqualTo,
    StrictEqualTo,
    NotStrictEqualTo,
    LeftShift,
    RightShift,
    RightShiftUnsigned,
    LogicalOr,
    LogicalAnd,
    InstanceOf,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    TypeOf,
    LogicalNot,
    Sub,
    Add,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expression<'a> {
    Float(f64),
    Boolean(bool),
    String(String),
    Null,
    Undefined,
    NewWithArgs {
        target: Box<Expression<'a>>,
        parameters: Vec<Expression<'a>>,
    },
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
    Inc {
        reference: Reference<'a>,
        pre: bool,
    },
    Dec {
        reference: Reference<'a>,
        pre: bool,
    },
    Shift {
        unsigned: bool,
        left: bool,
    },
    BinaryExpression {
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
        operator: BinaryOperator,
    },
    UnaryExpression {
        value: Box<Expression<'a>>,
        operator: UnaryOperator,
    },
    Function {
        name: Option<&'a str>,
        arguments: Vec<&'a str>,
        statements: BlockStatement<'a>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ReturnStatement<'a> {
    pub(crate) expression: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct VarStatement<'a> {
    pub(crate) declarations: Vec<VarDeclaration<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct VarDeclaration<'a> {
    pub(crate) identifier: &'a str,
    pub(crate) expression: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct WhileStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) loop_block: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct IfStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) true_block: Box<Statement<'a>>,
    pub(crate) false_block: Option<Box<Statement<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FunctionStatement<'a> {
    pub(crate) identifier: &'a str,
    pub(crate) arguments: Vec<&'a str>,
    pub(crate) statements: BlockStatement<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TryStatement<'a> {
    pub(crate) try_block: BlockStatement<'a>,
    pub(crate) catch_binding: Option<&'a str>,
    pub(crate) catch_block: Option<BlockStatement<'a>>,
    pub(crate) finally_block: Option<BlockStatement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ThrowStatement<'a> {
    pub(crate) expression: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct BlockStatement<'a> {
    pub(crate) statements: Vec<Statement<'a>>,
}

impl<'a> From<Statement<'a>> for BlockStatement<'a> {
    fn from(statement: Statement<'a>) -> Self {
        BlockStatement {
            statements: vec![statement],
        }
    }
}

impl<'a> From<Box<Statement<'a>>> for BlockStatement<'a> {
    fn from(statement: Box<Statement<'a>>) -> Self {
        BlockStatement {
            statements: vec![*statement],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ForStatement<'a> {
    VarList {
        expression: Option<Expression<'a>>,
        vars: Option<VarStatement<'a>>,
        condition: Option<Expression<'a>>,
        operation: Option<Expression<'a>>,
        block: Box<Statement<'a>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement<'a> {
    Block(BlockStatement<'a>),
    Function(FunctionStatement<'a>),
    If(IfStatement<'a>),
    Return(ReturnStatement<'a>),
    While(WhileStatement<'a>),
    Var(VarStatement<'a>),
    Expression(Expression<'a>),
    Try(TryStatement<'a>),
    For(ForStatement<'a>),
    Break,
    Continue,
    ThrowStatement(ThrowStatement<'a>),
}

#[derive(Debug, PartialEq)]
pub struct ParsedModule<'a> {
    pub(crate) block: Vec<Statement<'a>>,
}
