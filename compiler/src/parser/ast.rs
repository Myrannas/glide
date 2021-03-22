use crate::compiler::BucketEq;
use instruction_set::Constant;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Reference<'a> {
    This,
    Id(&'a str),
    Accessor {
        expression: Box<Expression<'a>>,
        accessor: &'a str,
        null_safe: bool,
    },
    ComputedAccessor {
        expression: Box<Expression<'a>>,
        accessor: Box<Expression<'a>>,
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
    In,
    Exponential,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    TypeOf,
    LogicalNot,
    Sub,
    Add,
    Delete,
}

#[allow(clippy::enum_variant_names)]
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
    Add {
        expressions: Vec<Expression<'a>>,
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
    ConditionalOperator {
        condition: Box<Expression<'a>>,
        if_true: Box<Expression<'a>>,
        if_false: Box<Expression<'a>>,
    },
    ObjectLiteral {
        attributes: Vec<(String, Expression<'a>)>,
    },
    ArrayLiteral {
        attributes: Vec<Expression<'a>>,
    },
    Inc {
        reference: Reference<'a>,
        pre: bool,
    },
    Dec {
        reference: Reference<'a>,
        pre: bool,
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

impl<'a> Expression<'a> {
    pub(crate) fn to_constant(&self, atoms: &mut Vec<String>) -> Option<Constant> {
        // println!("Trying to turn {:?} into constant", self);

        let constant = match self {
            Expression::Float(f) => Constant::Float(*f),
            Expression::Boolean(b) => Constant::Boolean(*b),
            Expression::String(s) => Constant::Atom(atoms.get_or_allocate(s.to_owned())),
            Expression::Null => Constant::Null,
            Expression::Undefined => Constant::Undefined,
            _ => return None,
        };

        Some(constant)
    }
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
pub(crate) struct ConstStatement<'a> {
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

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ClassStatement<'a> {
    pub(crate) name: &'a str,
    pub(crate) extends: Option<Expression<'a>>,
    pub(crate) members: Vec<ClassMember<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ClassMember<'a> {
    Constructor(FunctionStatement<'a>),
    Function(FunctionStatement<'a>),
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
    For {
        expression: Option<Expression<'a>>,
        vars: Option<VarStatement<'a>>,
        condition: Option<Expression<'a>>,
        operation: Option<Expression<'a>>,
        block: Box<Statement<'a>>,
    },
    ForIn {
        identifier: &'a str,
        expression: Expression<'a>,
        block: BlockStatement<'a>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement<'a> {
    Block(BlockStatement<'a>),
    Function(FunctionStatement<'a>),
    If(IfStatement<'a>),
    Return(ReturnStatement<'a>),
    Class(ClassStatement<'a>),
    While(WhileStatement<'a>),
    Var(VarStatement<'a>),
    Const(ConstStatement<'a>),
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
