use crate::ops::{
    add, bind, call, call_new, catch, cjmp, div, duplicate, eq, get, get_null_safe, gt, gte,
    increment, instance_of, jmp, land, lnot, load, load_this, lor, lshift, lt, lte, modulo, mul,
    ne, not_strict_eq, ret, rshift, rshift_u, set, strict_eq, sub, throw_value, truncate, type_of,
    uncatch, Instruction,
};

use crate::parser::ast::{
    BinaryOperator, BlockStatement, Expression, ForStatement, FunctionStatement, IfStatement,
    ParsedModule, Reference, ReturnStatement, Statement, ThrowStatement, TryStatement,
    UnaryOperator, VarDeclaration, VarStatement, WhileStatement,
};
use crate::result::{InternalError, StaticJsResult, SyntaxError};
use crate::value::StaticValue;
use crate::value::StaticValue::Local;
use crate::vm::{Function, FunctionInner, JsThread, Module};
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;
use StaticValue::{Branch, Capture, Jump};

pub struct Chunk {
    pub(crate) instructions: Vec<Instruction>,
}

impl Chunk {
    fn new() -> Chunk {
        Chunk {
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FrameLocals {
    locals: Vec<String>,
    parent: Option<Rc<RefCell<FrameLocals>>>,
}

struct Frame {
    chunks: Vec<Chunk>,
    functions: Vec<FunctionInner>,
    local_size: usize,
    locals: Rc<RefCell<FrameLocals>>,
}

trait Bucket<T> {
    fn allocate(&mut self, value: T) -> usize;
}

impl<T> Bucket<T> for Vec<T> {
    fn allocate(&mut self, value: T) -> usize {
        let len = self.len();
        self.push(value);
        len
    }
}

impl Frame {
    fn new_chunk(mut self, options: CompilerOptions) -> ChunkBuilder {
        let chunk = Chunk::new();

        let offset = self.chunks.allocate(chunk);

        ChunkBuilder {
            frame: self,
            chunk_index: offset,
            options,
        }
    }
}

struct ChunkBuilder {
    frame: Frame,
    chunk_index: usize,
    options: CompilerOptions,
}

impl<'b> ChunkBuilder {
    fn append(
        mut self,
        instr: for<'c, 'd, 'e> fn(&Option<StaticValue>, &'e mut JsThread<'c>),
    ) -> Self {
        self.frame.chunks[self.chunk_index]
            .instructions
            .push(Instruction {
                instr,
                constant: None,
            });

        self
    }

    fn append_with_constant(
        mut self,
        instr: for<'c, 'e> fn(&Option<StaticValue>, &'e mut JsThread<'c>),
        constant: StaticValue,
    ) -> Self {
        self.frame.chunks[self.chunk_index]
            .instructions
            .push(Instruction {
                instr,
                constant: Some(constant),
            });

        self
    }

    fn allocate_local(&mut self, id: &str) -> usize {
        self.frame.local_size += 1;
        self.frame
            .locals
            .borrow_mut()
            .locals
            .allocate(id.to_owned())
    }

    fn switch_to(self, chunk_index: usize) -> Self {
        ChunkBuilder {
            frame: self.frame,
            chunk_index,
            options: self.options,
        }
    }
}

enum Resolution {
    Resolved { local: usize },
    Capture { frame: usize, local: usize },
    Unresolved,
}

trait Locals {
    fn resolve_identifier(&self, id: &str) -> Resolution;
    fn allocate_identifier(&self, id: &str) -> usize;
    fn new_root() -> Self;
    fn child(&self) -> Self;
}

impl<'a> Locals for Rc<RefCell<FrameLocals>> {
    fn resolve_identifier(&self, id: &str) -> Resolution {
        let value = self.borrow();
        match (value.locals.iter().position(|v| *v == id), &value.parent) {
            (Some(position), _) => Resolution::Resolved {
                local: position as usize,
            },
            (None, Some(parent)) => match parent.resolve_identifier(id) {
                Resolution::Resolved { local } => Resolution::Capture { frame: 1, local },
                Resolution::Capture { frame, local } => Resolution::Capture {
                    frame: frame + 1,
                    local,
                },
                Resolution::Unresolved => Resolution::Unresolved,
            },
            (None, None) => Resolution::Unresolved,
        }
    }

    fn allocate_identifier(&self, id: &str) -> usize {
        self.borrow_mut().locals.allocate(id.to_owned())
    }

    fn new_root() -> Self {
        Rc::new(RefCell::new(FrameLocals {
            locals: vec![],
            parent: None,
        }))
    }

    fn child(&self) -> Self {
        Rc::new(RefCell::new(FrameLocals {
            locals: vec![],
            parent: Some(self.clone()),
        }))
    }
}

impl BinaryOperator {
    fn to_op(&self) -> for<'a, 'b, 'c, 'd> fn(&Option<StaticValue>, &'c mut JsThread<'a>) {
        match self {
            BinaryOperator::Add => add,
            BinaryOperator::Sub => sub,
            BinaryOperator::Mul => mul,
            BinaryOperator::Div => div,
            BinaryOperator::Mod => modulo,
            BinaryOperator::GreaterThan => gt,
            BinaryOperator::GreaterThanEqual => gte,
            BinaryOperator::LessThan => lt,
            BinaryOperator::LessThanEqual => lte,
            BinaryOperator::NotEqualTo => ne,
            BinaryOperator::EqualTo => eq,
            BinaryOperator::StrictEqualTo => strict_eq,
            BinaryOperator::NotStrictEqualTo => not_strict_eq,
            BinaryOperator::LeftShift => lshift,
            BinaryOperator::RightShift => rshift,
            BinaryOperator::RightShiftUnsigned => rshift_u,
            BinaryOperator::InstanceOf => instance_of,
            BinaryOperator::LogicalOr => panic!("Lor is handled separately"),
            BinaryOperator::LogicalAnd => panic!("Land is handled separately"),
        }
    }
}

impl UnaryOperator {
    fn to_op(&self) -> for<'a, 'b, 'c, 'd> fn(&Option<StaticValue>, &'c mut JsThread<'a>) {
        match self {
            UnaryOperator::TypeOf => type_of,
            UnaryOperator::LogicalNot => lnot,
            UnaryOperator::Sub => sub,
            UnaryOperator::Add => todo!("Not implemented"),
        }
    }
}

enum BreakStack {
    Root,
    Child {
        break_chunk: usize,
        continue_chunk: usize,
    },
}

impl BreakStack {
    fn new() -> BreakStack {
        BreakStack::Root
    }

    fn child(&self, break_chunk: usize, continue_chunk: usize) -> BreakStack {
        BreakStack::Child {
            break_chunk,
            continue_chunk,
        }
    }

    fn get_break(&self) -> StaticJsResult<usize> {
        match self {
            BreakStack::Root => SyntaxError::new("Illegal break statement").into(),
            BreakStack::Child { break_chunk, .. } => Ok(*break_chunk),
        }
    }

    fn get_continue(&self) -> StaticJsResult<usize> {
        match self {
            BreakStack::Root => SyntaxError::new("Illegal continue statement").into(),
            BreakStack::Child { continue_chunk, .. } => Ok(*continue_chunk),
        }
    }
}

impl<'a, 'c> ChunkBuilder {
    #[inline]
    fn resolve_identifier(&self, id: &str) -> Resolution {
        self.frame.locals.resolve_identifier(id)
    }

    #[inline]
    fn allocate_chunk(&mut self) -> usize {
        self.frame.chunks.allocate(Chunk {
            instructions: Vec::new(),
        })
    }

    fn compile_expression(
        mut self,
        input: Expression<'a>,
        // options: CompilerOptions,
    ) -> StaticJsResult<Self> {
        let chunk = match input {
            Expression::BinaryExpression {
                left,
                right,
                operator: BinaryOperator::LogicalOr,
            } => {
                let right_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                self.compile_expression(*left)?
                    .append_with_constant(lor, Branch(next_index, right_index))
                    .switch_to(right_index)
                    .compile_expression(*right)?
                    .append_with_constant(jmp, Jump(next_index))
                    .switch_to(next_index)
            }
            Expression::BinaryExpression {
                left,
                right,
                operator: BinaryOperator::LogicalAnd,
            } => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(land),
            Expression::BinaryExpression {
                left,
                right,
                operator,
            } => self
                .compile_expression(*right)?
                .compile_expression(*left)?
                .append(operator.to_op()),
            Expression::UnaryExpression {
                operator: UnaryOperator::Add,
                ..
            } => return InternalError::new_stackless("Add unary operator not implemented").into(),
            Expression::UnaryExpression { value, operator } => {
                self.compile_expression(*value)?.append(operator.to_op())
            }
            Expression::Float(value) => self.append_with_constant(load, StaticValue::Float(value)),
            Expression::Boolean(value) => {
                self.append_with_constant(load, StaticValue::Boolean(value))
            }
            Expression::String(str) => self.append_with_constant(load, StaticValue::String(str)),
            Expression::Inc { reference, .. } => self
                .compile_expression(Expression::Reference(reference))?
                .append_with_constant(increment, StaticValue::Float(1.0)),
            Expression::Dec { reference, .. } => self
                .compile_expression(Expression::Reference(reference))?
                .append_with_constant(increment, StaticValue::Float(-1.0)),
            Expression::Null => self.append_with_constant(load, StaticValue::Null),
            Expression::Assign {
                assign_to: Reference::Id(id),
                expression,
            } => {
                let next = self.compile_expression(*expression)?;

                match next.resolve_identifier(id) {
                    Resolution::Resolved { local } => {
                        next.append_with_constant(bind, StaticValue::Local(local))
                    }
                    Resolution::Capture { frame, local } => {
                        next.append_with_constant(set, StaticValue::Capture { frame, local })
                    }
                    Resolution::Unresolved => next
                        .append_with_constant(load, StaticValue::GlobalThis)
                        .append_with_constant(set, StaticValue::String(id.to_string())),
                }
            }
            Expression::Assign {
                assign_to:
                    Reference::Accessor {
                        accessor,
                        expression: target,
                        ..
                    },
                expression,
            } => self
                .compile_expression(*target)?
                .compile_expression(*expression)?
                .append_with_constant(set, StaticValue::String(accessor.to_owned())),
            Expression::Call {
                expression,
                parameters,
            } => {
                let args_len = parameters.len();

                parameters
                    .into_iter()
                    .try_fold(self, |next, expression| next.compile_expression(expression))?
                    .compile_expression(*expression)?
                    .append_with_constant(call, StaticValue::Float(args_len as f64))
            }

            Expression::NewWithArgs { target, parameters } => {
                // TODO ME

                let args_len = parameters.len();

                parameters
                    .into_iter()
                    .try_fold(self, |next, expression| next.compile_expression(expression))?
                    .compile_expression(*target)?
                    .append_with_constant(call_new, StaticValue::Float(args_len as f64))
            }
            Expression::ObjectLiteral { attributes } => {
                let next = self.append_with_constant(load, StaticValue::Object);

                attributes
                    .into_iter()
                    .try_fold(next, |builder, (attr, expression)| {
                        builder
                            .append(duplicate)
                            .compile_expression(expression)
                            .map(|builder| {
                                builder.append_with_constant(
                                    set,
                                    StaticValue::String(attr.to_string()),
                                )
                            })
                    })?
            }
            Expression::Reference(Reference::This) => self.append(load_this),
            Expression::Reference(Reference::Id(id)) => match self.resolve_identifier(id) {
                Resolution::Resolved { local } => self.append_with_constant(load, Local(local)),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => self.append_with_constant(
                    load,
                    Capture {
                        frame: capture_frame,
                        local,
                    },
                ),
                Resolution::Unresolved => self
                    .append_with_constant(load, StaticValue::GlobalThis)
                    .append_with_constant(get, StaticValue::String(id.to_string())),
            },
            Expression::Reference(Reference::Accessor {
                expression,
                accessor,
                null_safe,
            }) => {
                let next = self.compile_expression(*expression)?;

                if null_safe {
                    next.append_with_constant(
                        get_null_safe,
                        StaticValue::String(accessor.to_string()),
                    )
                } else {
                    next.append_with_constant(get, StaticValue::String(accessor.to_string()))
                }
            }
            Expression::Function {
                name: identifier,
                statements: BlockStatement { statements },
                arguments,
            } => {
                let identifier = identifier.unwrap_or("(anonymous)");
                let function = self.frame.functions.allocate(FunctionInner {
                    chunks: vec![],
                    stack_size: 0,
                    local_size: 0,
                    functions: vec![],
                    name: identifier.to_owned(),
                    locals: Locals::new_root(),
                });

                let mut next = self.append_with_constant(load, StaticValue::Function(function));

                let parent_locals = next.frame.locals.clone();
                let frame = &mut next.frame;

                let Frame {
                    chunks,
                    functions,
                    local_size,
                    locals,
                } = compile_function(
                    identifier,
                    parent_locals,
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?;

                frame.functions[function].chunks = chunks;
                frame.functions[function].local_size = local_size;
                frame.functions[function].functions =
                    functions.into_iter().map(|v| v.into()).collect();
                frame.functions[function].locals = locals;

                next
            }
            Expression::Undefined => self.append_with_constant(load, StaticValue::Undefined),
            node => {
                return InternalError::new_stackless(format!("Unexpected expression {:?}", node))
                    .into()
            }
        };

        Ok(chunk)
    }

    fn compile_statement<'b>(
        mut self,
        input: Statement<'a>,
        next_block: Option<usize>,
        break_stack: &'b BreakStack,
    ) -> StaticJsResult<Self> {
        let chunk = match input {
            Statement::Return(ReturnStatement { expression }) => {
                if let Some(expression) = expression {
                    self.compile_expression(expression)?.append(ret)
                } else {
                    self.append_with_constant(ret, StaticValue::Undefined)
                }
            }
            Statement::Var(VarStatement { declarations }) => {
                let mut next = self;

                for VarDeclaration {
                    identifier,
                    expression,
                } in declarations
                {
                    let idx = next.allocate_local(identifier);

                    next = next
                        .compile_expression(expression.unwrap_or(Expression::Undefined))?
                        .append_with_constant(bind, StaticValue::Local(idx))
                        .append(truncate);

                    if !next.options.module {
                        next = next
                            .append_with_constant(load, StaticValue::GlobalThis)
                            .append_with_constant(load, StaticValue::Local(idx))
                            .append_with_constant(set, StaticValue::String(identifier.to_owned()))
                    }
                }

                next
            }
            Statement::For(ForStatement::VarList {
                expression,
                operation,
                condition,
                block,
                vars,
            }) => {
                let condition_index = self.allocate_chunk();
                let operation_index = self.allocate_chunk();
                let block_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                let mut next = self;

                if let Some(vars) = vars {
                    next = next.compile_statement(Statement::Var(vars), next_block, break_stack)?;
                } else if let Some(expression) = expression {
                    next = next.compile_expression(expression)?;
                }

                next = next
                    .append_with_constant(jmp, Jump(condition_index))
                    .switch_to(condition_index);

                if let Some(condition) = condition {
                    next = next
                        .compile_expression(condition)?
                        .append_with_constant(cjmp, Branch(block_index, next_index));
                } else {
                    next = next.append_with_constant(jmp, Jump(block_index));
                }

                let next_break_stack = &break_stack.child(next_index, condition_index);
                next = next
                    .switch_to(block_index)
                    .compile_block(block, operation_index, next_break_stack)?
                    .append_with_constant(jmp, Jump(operation_index))
                    .switch_to(operation_index);

                if let Some(operation) = operation {
                    next = next.compile_expression(operation)?
                }

                next.append_with_constant(jmp, Jump(condition_index))
                    .switch_to(next_index)
            }
            Statement::Function(FunctionStatement {
                identifier,
                arguments,
                statements: BlockStatement { statements },
            }) => {
                let local = self.allocate_local(identifier);
                let function = self.frame.functions.allocate(FunctionInner {
                    chunks: vec![],
                    stack_size: 0,
                    local_size: 0,
                    functions: vec![],
                    locals: Locals::new_root(),
                    name: identifier.to_owned(),
                });

                let mut next = self
                    .append_with_constant(load, StaticValue::Function(function))
                    .append_with_constant(bind, Local(local));

                if !next.options.module {
                    next = next
                        .append_with_constant(load, StaticValue::GlobalThis)
                        .append_with_constant(load, StaticValue::Local(local))
                        .append_with_constant(set, StaticValue::String(identifier.to_owned()));
                }

                let parent_locals = next.frame.locals.clone();
                let frame = &mut next.frame;

                let Frame {
                    chunks,
                    functions,
                    local_size,
                    locals,
                } = compile_function(
                    identifier,
                    parent_locals,
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?;

                frame.functions[function].chunks = chunks;
                frame.functions[function].local_size = local_size;
                frame.functions[function].functions =
                    functions.into_iter().map(From::from).collect();
                frame.functions[function].locals = locals;

                next
            }
            Statement::Expression(expression) => {
                self.compile_expression(expression)?.append(truncate)
            }
            Statement::If(IfStatement {
                condition,
                true_block,
                false_block,
            }) => {
                let next_index = self.allocate_chunk();
                let if_index = self.allocate_chunk();

                let else_index = if false_block.is_some() {
                    self.allocate_chunk()
                } else {
                    next_index
                };

                let mut next = self
                    .compile_expression(condition)?
                    .append_with_constant(cjmp, Branch(if_index, else_index));

                next =
                    next.switch_to(if_index)
                        .compile_block(true_block, next_index, break_stack)?;

                if let Some(else_block) = false_block {
                    next = next.switch_to(else_index).compile_block(
                        else_block,
                        next_index,
                        break_stack,
                    )?
                };

                return Ok(next.switch_to(next_index));
            }
            Statement::While(WhileStatement {
                condition,
                loop_block,
            }) => {
                let condition_index = self.allocate_chunk();
                let block_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                let next_break_stack = &break_stack.child(next_index, condition_index);

                self.append_with_constant(jmp, Jump(condition_index))
                    .switch_to(condition_index)
                    .compile_expression(condition)?
                    .append_with_constant(cjmp, Branch(block_index, next_index))
                    .switch_to(block_index)
                    .compile_block(loop_block, condition_index, next_break_stack)?
                    .switch_to(next_index)
            }
            Statement::Try(TryStatement {
                try_block,
                finally_block,
                catch_block,
                catch_binding,
            }) => {
                let next_index = self.allocate_chunk();
                let try_index = self.allocate_chunk();
                let catch_index = self.allocate_chunk();
                let finally_index = self.allocate_chunk();

                let mut next = self
                    .append_with_constant(jmp, Jump(try_index))
                    .switch_to(try_index);

                if catch_block.is_some() {
                    next = next.append_with_constant(catch, Jump(catch_index));
                }

                next = next.compile_block(try_block, finally_index, break_stack)?;

                if let Some(catch_block) = catch_block {
                    next = next.append(uncatch).switch_to(catch_index);

                    if let Some(binding) = catch_binding {
                        let binding = next.allocate_local(binding);
                        next = next.append_with_constant(bind, Local(binding));
                    }

                    next = next.compile_block(catch_block, finally_index, break_stack)?;
                }

                next.switch_to(finally_index)
                    .compile_block(
                        finally_block.unwrap_or(BlockStatement { statements: vec![] }),
                        next_index,
                        break_stack,
                    )?
                    .switch_to(next_index)
            }
            Statement::ThrowStatement(ThrowStatement { expression }) => {
                self.compile_expression(expression)?.append(throw_value)
            }
            Statement::Block(block) => {
                self.compile_block(block, next_block.unwrap_or(usize::MAX), break_stack)?
            }
            Statement::Break => self.append_with_constant(jmp, Jump(break_stack.get_break()?)),
            Statement::Continue => {
                self.append_with_constant(jmp, Jump(break_stack.get_continue()?))
            }
        };

        Ok(chunk)
    }

    fn compile_block<'b, B: Into<BlockStatement<'a>>>(
        self,
        statements: B,
        next_block: usize,
        break_stack: &'b BreakStack,
    ) -> StaticJsResult<Self> {
        let next = statements
            .into()
            .statements
            .into_iter()
            .try_fold(self, |builder, statement| {
                builder.compile_statement(statement, Some(next_block), break_stack)
            })?
            .append_with_constant(jmp, Jump(next_block));

        Ok(next)
    }

    fn finalize(self) -> Frame {
        self.frame
    }
}

const DEFAULT_OPTIONS: CompilerOptions = CompilerOptions { module: true };

fn compile_function<'a>(
    _name: &'a str,
    parent: Rc<RefCell<FrameLocals>>,
    arguments: Vec<&'a str>,
    statements: Vec<Statement<'a>>,
    options: CompilerOptions,
) -> StaticJsResult<Frame> {
    let frame = Frame {
        chunks: vec![],
        functions: vec![],
        local_size: arguments.len() + 1,
        locals: parent.child(),
    };

    frame.locals.allocate_identifier("arguments");

    for argument in arguments {
        frame.locals.allocate_identifier(argument);
    }

    let mut chunk = frame.new_chunk(options);
    for statement in statements.into_iter() {
        chunk = chunk.compile_statement(statement, None, &BreakStack::new())?;
    }

    chunk = chunk.append_with_constant(ret, StaticValue::Undefined);

    // println!("Compilation output {:?}", locals);

    Ok(chunk.finalize())
}

#[derive(Copy, Clone)]
pub struct CompilerOptions {
    module: bool,
}

impl CompilerOptions {
    pub fn new() -> CompilerOptions {
        CompilerOptions { module: false }
    }
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions::new()
    }
}

pub fn compile_eval<'a>(
    function: &Function,
    _input_string: &str,
    input: ParsedModule<'a>,
) -> StaticJsResult<Function> {
    let frame = Frame {
        chunks: vec![],
        functions: vec![],
        local_size: 0,
        locals: function.inner().locals.child(),
    };

    let mut chunk = frame.new_chunk(CompilerOptions { module: true });
    for statement in input.block.into_iter() {
        chunk = chunk.compile_statement(statement, None, &BreakStack::new())?;
    }
    chunk = chunk.append(ret);

    let finalised = chunk.finalize();
    Ok(FunctionInner {
        locals: finalised.locals,
        functions: finalised.functions.into_iter().map(From::from).collect(),
        chunks: finalised.chunks,
        local_size: finalised.local_size,
        stack_size: 0,
        name: "eval".to_owned(),
    }
    .into())
}

pub fn compile<'a>(
    id: &'a str,
    input: ParsedModule<'a>,
    options: CompilerOptions,
) -> StaticJsResult<Module> {
    debug!("{:#?}", input);

    let frame = compile_function(id, Locals::new_root(), Vec::new(), input.block, options)?;

    Ok(Module {
        init: FunctionInner {
            chunks: frame.chunks,
            stack_size: 0,
            local_size: frame.local_size,
            functions: frame.functions.into_iter().map(From::from).collect(),
            locals: frame.locals,
            name: "(anonymous)".to_owned(),
        }
        .into(),
    })
}
