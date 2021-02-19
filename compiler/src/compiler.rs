use crate::ops::{
    add, bind, call, cjmp, div, get, get_null_safe, gt, gte, jmp, land, lnot, load, lor, lt, lte,
    modulo, mul, neg, not_strict_eq, ret, set, strict_eq, sub, truncate, type_of, ControlFlow,
    Instruction, RuntimeFrame,
};
use crate::parser::ast::{
    Expression, FunctionStatement, IfStatement, ParsedModule, Reference, ReturnStatement,
    Statement, TryStatement, VarStatement, WhileStatement,
};
use crate::value::StaticValue;
use crate::value::StaticValue::Local;
use crate::vm::{Function, Module};
use anyhow::{bail, Context, Result};
use log::debug;
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

#[derive(Debug)]
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
    fn new_chunk<'a, 'b>(&'a mut self, locals: &'b mut FrameLocals<'a>) -> ChunkBuilder<'a, 'b> {
        let chunk = Chunk::new();

        let offset = self.chunks.allocate(chunk);

        ChunkBuilder {
            frame: self,
            chunk_index: offset,
            locals,
        }
    }
}

struct ChunkBuilder<'a, 'b> {
    frame: &'b mut Frame,
    locals: &'b mut FrameLocals<'a>,
    chunk_index: usize,
}

impl<'a, 'b> ChunkBuilder<'a, 'b> {
    fn append(
        &mut self,
        instr: for<'c, 'd, 'e> fn(
            &Option<StaticValue>,
            &'e mut RuntimeFrame<'c, 'd>,
        ) -> ControlFlow<'c>,
    ) {
        self.frame.chunks[self.chunk_index]
            .instructions
            .push(Instruction {
                instr,
                constant: None,
            })
    }

    fn append_with_constant(
        &mut self,
        instr: for<'c, 'd, 'e> fn(
            &Option<StaticValue>,
            &'e mut RuntimeFrame<'c, 'd>,
        ) -> ControlFlow<'c>,
        constant: StaticValue,
    ) {
        self.frame.chunks[self.chunk_index]
            .instructions
            .push(Instruction {
                instr,
                constant: Some(constant),
            })
    }

    fn finalize(self) -> usize {
        self.chunk_index
    }

    fn allocate_local(&mut self, id: &'a str) -> usize {
        self.frame.local_size += 1;
        self.locals.locals.allocate(id)
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

fn compile_expression<'a, 'b, 'c>(
    input: Expression<'a>,
    chunk: &'b mut ChunkBuilder<'a, 'c>,
) -> Result<()> {
    match input {
        Expression::Add(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(add)
        }
        Expression::Sub(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(sub)
        }
        Expression::Mul(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(mul)
        }
        Expression::Div(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(div)
        }
        Expression::Mod(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(modulo)
        }
        Expression::Float(value) => chunk.append_with_constant(load, StaticValue::Float(value)),
        Expression::Boolean(value) => chunk.append_with_constant(load, StaticValue::Boolean(value)),
        Expression::LessThan(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(lt)
        }
        Expression::LessThanEqual(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(lte)
        }
        Expression::GreaterThan(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(gt)
        }
        Expression::GreaterThanEqual(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(gte)
        }
        Expression::LogicalOr(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(lor)
        }
        Expression::LogicalAnd(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;
            chunk.append(land)
        }
        Expression::LogicalNot(value) => {
            compile_expression(*value, chunk)?;
            chunk.append(lnot)
        }
        Expression::Neg(value) => {
            compile_expression(*value, chunk)?;
            chunk.append(neg)
        }
        Expression::TypeOf(value) => {
            compile_expression(*value, chunk)?;
            chunk.append(type_of)
        }
        Expression::String(str) => {
            chunk.append_with_constant(load, StaticValue::String(str.to_owned()))
        }
        Expression::Assign {
            assign_to: Reference::Id(id),
            expression,
        } => {
            compile_expression(*expression, chunk)?;
            match chunk.locals.resolve_identifier(id) {
                Resolution::Resolved { local } => {
                    chunk.append_with_constant(bind, StaticValue::Local(local))
                }
                Resolution::Capture { .. } => {
                    todo!("Can't mutate other contexts atm")
                }
                Resolution::Unresolved => {
                    bail!("Reference unresolved {}", id)
                }
            };
        }
        Expression::Assign {
            assign_to:
                Reference::Accessor {
                    accessor,
                    expression: target,
                    ..
                },
            expression,
        } => {
            compile_expression(*target, chunk)?;
            compile_expression(*expression, chunk)?;

            chunk.append_with_constant(set, StaticValue::String(accessor.to_owned()))
        }
        Expression::Call {
            expression,
            parameters,
        } => {
            compile_expression(*expression, chunk)?;

            let args_len = parameters.len();
            for argument in parameters {
                compile_expression(argument, chunk)?;
            }

            chunk.append_with_constant(call, StaticValue::Float(args_len as f64))
        }

        Expression::NewWithArgs { target, parameters } => {
            // TODOME
            compile_expression(*target, chunk)?;

            let args_len = parameters.len();
            for argument in parameters {
                compile_expression(argument, chunk)?;
            }

            chunk.append_with_constant(call, StaticValue::Float(args_len as f64))
        }
        Expression::ObjectLiteral { attributes } => {
            chunk.append_with_constant(load, StaticValue::Object);

            for (attr, expression) in attributes {
                compile_expression(expression, chunk)?;
                chunk.append_with_constant(set, StaticValue::String(attr.to_string()))
            }
        }
        Expression::Reference(Reference::Id(id)) => {
            match chunk.locals.resolve_identifier(id) {
                Resolution::Resolved { local } => chunk.append_with_constant(load, Local(local)),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => {
                    chunk.append_with_constant(load, Capture(capture_frame, local));
                }
                Resolution::Unresolved => {
                    chunk.append_with_constant(load, StaticValue::GlobalThis);
                    chunk.append_with_constant(get, StaticValue::String(id.to_string()));
                }
            };
        }
        Expression::Reference(Reference::Accessor {
            expression,
            accessor,
            null_safe,
        }) => {
            compile_expression(*expression, chunk)?;

            if null_safe {
                chunk.append_with_constant(get_null_safe, StaticValue::String(accessor.to_string()))
            } else {
                chunk.append_with_constant(get, StaticValue::String(accessor.to_string()))
            }
        }
        Expression::StrictEqualTo(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;

            chunk.append(strict_eq)
        }
        Expression::NotStrictEqualTo(left, right) => {
            compile_expression(*left, chunk)?;
            compile_expression(*right, chunk)?;

            chunk.append(not_strict_eq);
        }
        Expression::Function {
            name: identifier,
            statements,
            arguments,
        } => {
            let identifier = identifier.unwrap_or("<>");
            let function = chunk.frame.functions.allocate(Function {
                chunks: vec![],
                stack_size: 0,
                local_size: 0,
                functions: vec![],
                name: identifier.to_owned(),
            });

            chunk.append_with_constant(load, StaticValue::Function(function));

            let frame = &mut chunk.frame;

            let Frame {
                chunks,
                functions,
                local_size,
            } = compile_function(identifier, &chunk.locals, arguments, statements)?;

            frame.functions[function].chunks = chunks;
            frame.functions[function].local_size = local_size;
            frame.functions[function].functions = functions;
        }
        Expression::Undefined => chunk.append_with_constant(load, StaticValue::Undefined),
        node => panic!("Unsupported node {:#?}", node),
    }

    Ok(())
}

fn compile_statement<'a, 'b>(
    input: Statement<'a>,
    mut chunk: ChunkBuilder<'a, 'b>,
) -> Result<ChunkBuilder<'a, 'b>> {
    match input {
        Statement::Return(ReturnStatement { expression }) => {
            if let Some(expression) = expression {
                compile_expression(expression, &mut chunk)?;
                chunk.append(ret)
            } else {
                chunk.append_with_constant(ret, StaticValue::Undefined)
            }
        }
        Statement::Var(VarStatement {
            identifier,
            expression,
        }) => {
            let idx = chunk.allocate_local(identifier);
            compile_expression(expression, &mut chunk)?;
            chunk.append_with_constant(bind, StaticValue::Local(idx));
            chunk.append(truncate)
        }
        Statement::Function(FunctionStatement {
            identifier,
            arguments,
            statements,
        }) => {
            let local = chunk.allocate_local(identifier);
            let function = chunk.frame.functions.allocate(Function {
                chunks: vec![],
                stack_size: 0,
                local_size: 0,
                functions: vec![],
                name: identifier.to_owned(),
            });

            chunk.append_with_constant(load, StaticValue::Function(function));
            chunk.append_with_constant(bind, Local(local));

            let frame = &mut chunk.frame;

            let Frame {
                chunks,
                functions,
                local_size,
            } = compile_function(identifier, &chunk.locals, arguments, statements)?;

            frame.functions[function].chunks = chunks;
            frame.functions[function].local_size = local_size;
            frame.functions[function].functions = functions;
        }
        Statement::Expression(expression) => {
            compile_expression(expression, &mut chunk)?;
            chunk.append(truncate)
        }
        Statement::If(IfStatement {
            condition,
            true_block,
            false_block,
        }) => {
            compile_expression(condition, &mut chunk)?;

            let chunk_index = chunk.frame.chunks.allocate(Chunk::new());

            let left = compile_block(
                true_block,
                &mut chunk.frame,
                &mut chunk.locals,
                None,
                chunk_index,
            )?;
            let right = if let Some(else_block) = false_block {
                compile_block(
                    else_block,
                    &mut chunk.frame,
                    &mut chunk.locals,
                    None,
                    chunk_index,
                )?
            } else {
                chunk_index
            };

            chunk.append_with_constant(cjmp, StaticValue::Branch(left, right));

            let sibling = ChunkBuilder {
                locals: chunk.locals,
                frame: chunk.frame,
                chunk_index,
            };

            return Ok(sibling);
        }
        Statement::While(WhileStatement {
            condition,
            loop_block,
        }) => {
            let condition_chunk_index = chunk.frame.chunks.allocate(Chunk::new());
            let block_chunk_index = chunk.frame.chunks.allocate(Chunk::new());
            let next_chunk_index = chunk.frame.chunks.allocate(Chunk::new());
            chunk.append_with_constant(jmp, StaticValue::Jump(condition_chunk_index));

            let mut condition_chunk = ChunkBuilder {
                locals: chunk.locals,
                frame: chunk.frame,
                chunk_index: condition_chunk_index,
            };

            compile_expression(condition, &mut condition_chunk)?;
            condition_chunk.append_with_constant(
                cjmp,
                StaticValue::Branch(block_chunk_index, next_chunk_index),
            );

            compile_block(
                loop_block,
                &mut chunk.frame,
                &mut chunk.locals,
                Some(block_chunk_index),
                condition_chunk_index,
            )?;

            let sibling = ChunkBuilder {
                locals: chunk.locals,
                frame: chunk.frame,
                chunk_index: next_chunk_index,
            };

            return Ok(sibling);
        }
        Statement::Try(TryStatement {
            try_block,
            finally_block,
            ..
        }) => {
            let try_block_index = chunk.frame.chunks.allocate(Chunk::new());
            let next_chunk_index = chunk.frame.chunks.allocate(Chunk::new());
            let finally_block_index = if finally_block.is_some() {
                chunk.frame.chunks.allocate(Chunk::new())
            } else {
                next_chunk_index
            };

            chunk.append_with_constant(jmp, StaticValue::Jump(try_block_index));

            compile_block(
                try_block,
                &mut chunk.frame,
                &mut chunk.locals,
                Some(try_block_index),
                finally_block_index,
            )?;

            if let Some(finally) = finally_block {
                compile_block(
                    finally,
                    &mut chunk.frame,
                    &mut chunk.locals,
                    Some(finally_block_index),
                    next_chunk_index,
                )?;
            }

            let sibling = ChunkBuilder {
                locals: chunk.locals,
                frame: chunk.frame,
                chunk_index: next_chunk_index,
            };

            return Ok(sibling);
            // need to implement catch
        }
    }

    Ok(chunk)
}

fn compile_block<'a, 'b>(
    block: Vec<Statement<'a>>,
    frame: &'b mut Frame,
    locals: &'b mut FrameLocals<'a>,
    chunk_index: Option<usize>,
    next_chunk: usize,
) -> Result<usize> {
    let chunk_index = chunk_index.unwrap_or_else(|| frame.chunks.allocate(Chunk::new()));
    let mut sibling = ChunkBuilder {
        locals,
        frame,
        chunk_index,
    };

    for ast in block.into_iter() {
        sibling = compile_statement(ast, sibling)?;
    }

    sibling.append_with_constant(jmp, StaticValue::Jump(next_chunk));

    Ok(sibling.finalize())
}

fn compile_function<'a>(
    name: &'a str,
    parent: &'a FrameLocals<'a>,
    arguments: Vec<&'a str>,
    statements: Vec<Statement<'a>>,
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

    let mut chunk = frame.new_chunk(&mut locals);
    for statement in statements.into_iter() {
        chunk = compile_statement(statement, chunk)
            .with_context(|| format!("Error compiling function {}", name))?;
    }

    chunk.append_with_constant(ret, StaticValue::Undefined);

    // println!("Compilation output {:?}", locals);

    Ok(frame)
}

pub fn compile(id: &str, input: ParsedModule) -> Result<Module> {
    debug!("{:#?}", input);

    let frame = compile_function(id, &FrameLocals::new_root(), Vec::new(), input.block)
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
