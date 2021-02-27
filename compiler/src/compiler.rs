use crate::ops::{
    add, bind, call, call_new, cjmp, div, get, get_null_safe, gt, gte, jmp, land, lnot, load,
    load_this, lor, lt, lte, modulo, mul, neg, not_strict_eq, ret, set, strict_eq, sub,
    throw_value, truncate, type_of, ControlFlow, Instruction, RuntimeFrame,
};
use crate::parser::ast::{
    Expression, FunctionStatement, IfStatement, ParsedModule, Reference, ReturnStatement,
    Statement, ThrowStatement, TryStatement, VarStatement, WhileStatement,
};
use crate::value::StaticValue;
use crate::value::StaticValue::Local;
use crate::vm::{Function, Module};
use anyhow::{bail, Context, Result};
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;
use StaticValue::Capture;

#[derive(Debug)]
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
struct FrameLocals<'a> {
    locals: Vec<&'a str>,
    parent: Option<&'a FrameLocals<'a>>,
}

struct Frame {
    chunks: Vec<Chunk>,
    functions: Vec<Function>,
    local_size: usize,
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
    fn new_chunk(mut self, options: CompilerOptions, locals: FrameLocals) -> ChunkBuilder {
        let chunk = Chunk::new();

        let offset = self.chunks.allocate(chunk);

        ChunkBuilder {
            frame: self,
            locals,
            chunk_index: offset,
            options,
        }
    }
}

struct ChunkBuilder<'a> {
    frame: Frame,
    locals: FrameLocals<'a>,
    chunk_index: usize,
    options: CompilerOptions,
}

impl<'a, 'b> ChunkBuilder<'a> {
    fn append(
        mut self,
        instr: for<'c, 'd, 'e> fn(
            &Option<StaticValue>,
            &'e mut RuntimeFrame<'c, 'd>,
        ) -> ControlFlow<'c>,
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
        instr: for<'c, 'd, 'e> fn(
            &Option<StaticValue>,
            &'e mut RuntimeFrame<'c, 'd>,
        ) -> ControlFlow<'c>,
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

    fn allocate_local(&mut self, id: &'a str) -> usize {
        self.frame.local_size += 1;
        self.locals.locals.allocate(id)
    }

    fn switch_to(mut self, chunk_index: usize) -> Self {
        ChunkBuilder {
            frame: self.frame,
            locals: self.locals,
            chunk_index,
            options: self.options.clone(),
        }
    }
}

enum Resolution {
    Resolved { local: usize },
    Capture { frame: usize, local: usize },
    Unresolved,
}

impl<'a> FrameLocals<'a> {
    fn resolve_identifier(&self, id: &str) -> Resolution {
        match (self.locals.iter().position(|v| *v == id), self.parent) {
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

    fn new_root() -> FrameLocals<'a> {
        FrameLocals {
            locals: vec![],
            parent: None,
        }
    }

    fn child(&self) -> FrameLocals<'_> {
        FrameLocals {
            locals: vec![],
            parent: Some(self),
        }
    }
}

impl<'a, 'c> ChunkBuilder<'a> {
    #[inline]
    fn resolve_identifier(&self, id: &str) -> Resolution {
        self.locals.resolve_identifier(id)
    }

    #[inline]
    fn allocate_chunk(&mut self) -> usize {
        self.frame.chunks.allocate(Chunk {
            instructions: Vec::new(),
        })
    }

    fn compile_expression<'b>(
        mut self,
        input: Expression<'a>,
        // options: CompilerOptions,
    ) -> Result<Self> {
        let chunk = match input {
            Expression::Add(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(add),
            Expression::Sub(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(sub),
            Expression::Mul(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(mul),
            Expression::Div(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(div),
            Expression::Mod(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(modulo),
            Expression::Float(value) => self.append_with_constant(load, StaticValue::Float(value)),
            Expression::Boolean(value) => {
                self.append_with_constant(load, StaticValue::Boolean(value))
            }
            Expression::LessThan(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(lt),
            Expression::LessThanEqual(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(lte),
            Expression::GreaterThan(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(gt),
            Expression::GreaterThanEqual(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(gte),
            Expression::LogicalOr(left, right) => {
                let right_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                self.compile_expression(*left)?
                    .append_with_constant(lor, StaticValue::Branch(next_index, right_index))
                    .switch_to(right_index)
                    .compile_expression(*right)?
                    .append_with_constant(jmp, StaticValue::Jump(next_index))
                    .switch_to(next_index)
            }
            Expression::LogicalAnd(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(land),
            Expression::LogicalNot(value) => self.compile_expression(*value)?.append(lnot),
            Expression::Neg(value) => self.compile_expression(*value)?.append(neg),
            Expression::TypeOf(value) => self.compile_expression(*value)?.append(type_of),
            Expression::String(str) => {
                self.append_with_constant(load, StaticValue::String(str.to_owned()))
            }
            Expression::Assign {
                assign_to: Reference::Id(id),
                expression,
            } => {
                let next = self.compile_expression(*expression)?;

                match next.resolve_identifier(id) {
                    Resolution::Resolved { local } => {
                        next.append_with_constant(bind, StaticValue::Local(local))
                    }
                    Resolution::Capture { .. } => {
                        todo!("Can't mutate other contexts atm")
                    }
                    Resolution::Unresolved => {
                        bail!("Reference unresolved {}", id)
                    }
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
                // TODOME

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
                        builder.compile_expression(expression).map(|builder| {
                            builder.append_with_constant(set, StaticValue::String(attr.to_string()))
                        })
                    })?
            }
            Expression::Reference(Reference::This) => self.append(load_this),
            Expression::Reference(Reference::Id(id)) => match self.resolve_identifier(id) {
                Resolution::Resolved { local } => self.append_with_constant(load, Local(local)),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => self.append_with_constant(load, Capture(capture_frame, local)),
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
            Expression::StrictEqualTo(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(strict_eq),
            Expression::NotStrictEqualTo(left, right) => self
                .compile_expression(*left)?
                .compile_expression(*right)?
                .append(not_strict_eq),
            Expression::Function {
                name: identifier,
                statements,
                arguments,
            } => {
                let identifier = identifier.unwrap_or("<>");
                let function = self.frame.functions.allocate(Function {
                    chunks: vec![],
                    stack_size: 0,
                    local_size: 0,
                    functions: vec![],
                    name: identifier.to_owned(),
                });

                let mut next = self.append_with_constant(load, StaticValue::Function(function));

                let frame = &mut next.frame;

                let Frame {
                    chunks,
                    functions,
                    local_size,
                } = compile_function(
                    identifier,
                    &next.locals,
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?;

                frame.functions[function].chunks = chunks;
                frame.functions[function].local_size = local_size;
                frame.functions[function].functions = functions;

                next
            }
            Expression::Undefined => self.append_with_constant(load, StaticValue::Undefined),
            node => panic!("Unsupported node {:#?}", node),
        };

        Ok(chunk)
    }

    fn compile_statement<'b>(mut self, input: Statement<'a>) -> Result<Self> {
        let chunk = match input {
            Statement::Return(ReturnStatement { expression }) => {
                if let Some(expression) = expression {
                    self.compile_expression(expression)?.append(ret)
                } else {
                    self.append_with_constant(ret, StaticValue::Undefined)
                }
            }
            Statement::Var(VarStatement {
                identifier,
                expression,
            }) => {
                let idx = self.allocate_local(identifier);

                let next = self
                    .compile_expression(expression)?
                    .append_with_constant(bind, StaticValue::Local(idx))
                    .append(truncate);

                if !next.options.module {
                    next.append_with_constant(load, StaticValue::GlobalThis)
                        .append_with_constant(load, StaticValue::Local(idx))
                        .append_with_constant(set, StaticValue::String(identifier.to_owned()))
                } else {
                    next
                }
            }
            Statement::Function(FunctionStatement {
                identifier,
                arguments,
                statements,
            }) => {
                let local = self.allocate_local(identifier);
                let function = self.frame.functions.allocate(Function {
                    chunks: vec![],
                    stack_size: 0,
                    local_size: 0,
                    functions: vec![],
                    name: identifier.to_owned(),
                });

                let mut next = self
                    .append_with_constant(load, StaticValue::Function(function))
                    .append_with_constant(bind, Local(local));

                if !next.options.module {
                    next = next
                        .append_with_constant(load, StaticValue::GlobalThis)
                        .append_with_constant(load, StaticValue::Function(function))
                        .append_with_constant(set, StaticValue::String(identifier.to_owned()));
                }

                let frame = &mut next.frame;

                let Frame {
                    chunks,
                    functions,
                    local_size,
                } = compile_function(
                    identifier,
                    &next.locals,
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?;

                frame.functions[function].chunks = chunks;
                frame.functions[function].local_size = local_size;
                frame.functions[function].functions = functions;

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
                    .append_with_constant(cjmp, StaticValue::Branch(if_index, else_index));

                next = next
                    .switch_to(if_index)
                    .compile_block(true_block, next_index)?;

                if let Some(else_block) = false_block {
                    next = next
                        .switch_to(else_index)
                        .compile_block(else_block, next_index)?
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

                self.append_with_constant(jmp, StaticValue::Jump(condition_index))
                    .switch_to(condition_index)
                    .compile_expression(condition)?
                    .append_with_constant(cjmp, StaticValue::Branch(block_index, next_index))
                    .switch_to(block_index)
                    .compile_block(loop_block, condition_index)?
                    .switch_to(next_index)
            }
            Statement::Try(TryStatement {
                try_block,
                finally_block,
                ..
            }) => {
                let next_index = self.allocate_chunk();
                let try_index = self.allocate_chunk();
                let finally_index = self.allocate_chunk();

                self.append_with_constant(jmp, StaticValue::Jump(try_index))
                    .switch_to(try_index)
                    .compile_block(try_block, finally_index)?
                    .switch_to(finally_index)
                    .compile_block(finally_block.unwrap_or(vec![]), next_index)?
            }
            Statement::ThrowStatement(ThrowStatement { expression }) => {
                self.compile_expression(expression)?.append(throw_value)
            }
        };

        Ok(chunk)
    }

    fn compile_block<'b>(mut self, block: Vec<Statement<'a>>, next_index: usize) -> Result<Self> {
        let next = block
            .into_iter()
            .try_fold(self, |builder, statement| {
                builder.compile_statement(statement)
            })?
            .append_with_constant(jmp, StaticValue::Jump(next_index));

        Ok(next)
    }

    fn finalize(self) -> Frame {
        self.frame
    }
}

const DEFAULT_OPTIONS: CompilerOptions = CompilerOptions { module: true };

fn compile_function<'a>(
    name: &'a str,
    parent: &'a FrameLocals<'a>,
    arguments: Vec<&'a str>,
    statements: Vec<Statement<'a>>,
    options: CompilerOptions,
) -> Result<Frame> {
    let mut frame = Frame {
        chunks: vec![],
        functions: vec![],
        local_size: arguments.len(),
    };

    let mut locals = parent.child();

    for argument in arguments {
        locals.locals.push(argument);
    }

    let mut chunk = frame.new_chunk(options, locals);
    for statement in statements.into_iter() {
        chunk = chunk
            .compile_statement(statement)
            .with_context(|| format!("Error compiling function {}", name))?;
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

pub fn compile(id: &str, input: ParsedModule, options: CompilerOptions) -> Result<Module> {
    debug!("{:#?}", input);

    let frame = compile_function(
        id,
        &FrameLocals::new_root(),
        Vec::new(),
        input.block,
        options,
    )
    .with_context(|| format!("Error compiling module {}", id))?;

    Ok(Module {
        init: Function {
            chunks: frame.chunks,
            stack_size: 0,
            local_size: frame.local_size,
            functions: frame.functions,
            name: "<init>".to_owned(),
        },
    })
}
