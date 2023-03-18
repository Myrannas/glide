mod binary_operator;
mod expression;
mod locals;
mod unary_operator;

use crate::compiler::locals::{LocalAllocator, Resolution};
use crate::parser::ast::Reference::{Accessor, Id};
use crate::parser::ast::{
    BlockStatement, ConstStatement, Expression, Field, FunctionStatement, IfStatement,
    ParsedModule, ReturnStatement, ThrowStatement, TryStatement, UnaryOperator, VarDeclaration,
    VarStatement, WhileStatement,
};
use crate::parser::parse_input_as;
use crate::parser::statements::class_statement::ClassMember;
use crate::parser::statements::decl_statement::DeclStatement;
use crate::parser::statements::for_statement::ForStatement;
use crate::parser::statements::statement::Statement;
use crate::parser::ParseContext;
use crate::result::{CompilerError, InternalError, Result, SyntaxError};
use instruction_set::Constant::Undefined;
use instruction_set::Environmental::GlobalThis;
use instruction_set::Instruction::*;
use instruction_set::{
    Chunk, Class, Constant, Environmental, Function, Instruction, Local, LocalInit, Module,
    Property,
};
use log::debug;
use std::ops::Index;

#[macro_export]
macro_rules! internal_error {
    ($format: literal, $($y:expr),+) => {
        return Err(CompilerError::InternalError(InternalError::new(format!(
            $format, ($($y),+)
        ))));
    };

    ($literal: literal) => {
        return Err(CompilerError::InternalError(InternalError::new($literal)));
    };
}

macro_rules! syntax_error {
    ($error: literal) => {
        return Err(CompilerError::SyntaxError(SyntaxError::new($error)));
    };
}

#[derive(Debug)]
pub(crate) struct Frame {
    pub(crate) chunks: Vec<Chunk>,
    pub(crate) functions: Vec<Function>,
    pub(crate) classes: Vec<Class>,
    pub(crate) atoms: Vec<String>,
    pub(crate) locals: LocalAllocator,
}

pub trait Bucket<T> {
    fn allocate(&mut self, value: T) -> usize;
}

pub trait BucketEq<T>: Bucket<T>
where
    T: PartialEq,
{
    fn get_or_allocate(&mut self, value: T) -> usize;
}

impl<T> Bucket<T> for Vec<T> {
    fn allocate(&mut self, value: T) -> usize {
        let len = self.len();
        self.push(value);
        len
    }
}

impl<T> BucketEq<T> for Vec<T>
where
    T: PartialEq,
{
    fn get_or_allocate(&mut self, value: T) -> usize {
        if let Some(existing) = self.iter().position(|v| v == &value) {
            existing
        } else {
            let position = self.len();
            self.push(value);
            position
        }
    }
}

impl Frame {
    fn new_chunk(mut self, options: CompilerOptions) -> ChunkBuilder {
        let chunk: Chunk = Default::default();

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
    fn compile(self, node: impl Compile) -> Result<Self> {
        node.compile(self)
    }

    fn append(mut self, instr: Instruction) -> Self {
        self.frame.chunks[self.chunk_index].push(instr);

        self
    }

    fn allocate_local(&mut self, id: &str, argument: bool) -> usize {
        let locals = &self.frame.locals.locals;

        let existing = locals
            .iter()
            .find(|Local { name, frame, .. }| name == id && *frame == 0);

        if let Some(existing) = existing {
            existing.local
        } else {
            self.frame
                .locals
                .allocate_identifier(id.to_owned(), argument)
        }
    }

    fn switch_to(self, chunk_index: usize) -> Self {
        ChunkBuilder {
            frame: self.frame,
            chunk_index,
            options: self.options,
        }
    }
}

// impl<'a> Locals for Rc<RefCell<FrameLocals>> {
//     fn resolve_identifier(&self, id: &str) -> Resolution {
//         let value = self.borrow();
//         match (value.locals.iter().position(|v| *v == id), &value.parent) {
//             (Some(position), _) => Resolution::Resolved {
//                 local: position as usize,
//             },
//             (None, Some(parent)) => match parent.resolve_identifier(id) {
//                 Resolution::Resolved { local } => Resolution::Capture { frame: 1, local },
//                 Resolution::Capture { frame, local } => Resolution::Capture {
//                     frame: frame + 1,
//                     local,
//                 },
//                 Resolution::Unresolved => Resolution::Unresolved,
//             },
//             (None, None) => Resolution::Unresolved,
//         }
//     }
//
//     fn allocate_identifier(&self, id: &str) -> usize {
//         self.borrow_mut().locals.allocate(id.to_owned())
//     }
//
//     fn new_root() -> Self {
//         Rc::new(RefCell::new(FrameLocals {
//             locals: vec![],
//             parent: None,
//         }))
//     }
//
//     fn child(&self) -> Self {
//         Rc::new(RefCell::new(FrameLocals {
//             locals: vec![],
//             parent: Some(self.clone()),
//         }))
//     }
// }

enum BreakStack {
    Root,
    Child {
        break_chunk: usize,
        continue_chunk: usize,
    },
}

fn freeze_instructions(mut chunks: Vec<Chunk>) -> Vec<Instruction> {
    let mut labels = Vec::with_capacity(chunks.len());

    for (chunk_index, chunk) in chunks.iter_mut().enumerate() {
        while let Some(Instruction::Jump { to }) = chunk.last() {
            if *to == chunk_index + 1 {
                chunk.pop();
            } else {
                break;
            }
        }
    }

    // println!("{:#?}", chunks);

    let mut offset = 0;
    for chunk in chunks.iter() {
        labels.push(offset);
        offset += chunk.len();
    }

    // println!("{:?}", labels);

    let result: Vec<Instruction> = chunks
        .into_iter()
        .flatten()
        .map(|i| match i {
            Instruction::Jump { to: usize::MAX } => Instruction::Jump { to: usize::MAX },
            Instruction::Jump { to } => Instruction::Jump { to: labels[to] },
            Instruction::CompareJump { if_true, if_false } => Instruction::CompareJump {
                if_true: labels[if_true],
                if_false: labels[if_false],
            },
            Instruction::LogicalOr { left, right } => Instruction::LogicalOr {
                left: labels[left],
                right: labels[right],
            },
            Instruction::LogicalAnd { left, right } => Instruction::LogicalAnd {
                left: labels[left],
                right: labels[right],
            },
            Instruction::Catch { chunk } => Instruction::Catch {
                chunk: labels[chunk],
            },
            Instruction::DropCatch { chunk } => Instruction::DropCatch {
                chunk: labels[chunk],
            },
            i => i,
        })
        .collect();

    // let results_with_indexes: Vec<(usize, &Instruction)> = result.iter().enumerate().collect();
    // println!("{:#?}", results_with_indexes);

    result
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

    fn get_break(&self) -> Result<usize> {
        match self {
            BreakStack::Root => syntax_error!("Illegal break statement"),
            BreakStack::Child { break_chunk, .. } => Ok(*break_chunk),
        }
    }

    fn get_continue(&self) -> Result<usize> {
        match self {
            BreakStack::Root => syntax_error!("Illegal continue statement"),
            BreakStack::Child { continue_chunk, .. } => Ok(*continue_chunk),
        }
    }
}

trait Compile {
    fn compile(self, builder: ChunkBuilder) -> Result<ChunkBuilder>;
}

impl<T: Compile> Compile for Option<T> {
    fn compile(self, builder: ChunkBuilder) -> Result<ChunkBuilder> {
        match self {
            Some(t) => t.compile(builder),
            None => Ok(builder),
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
        self.frame.chunks.allocate(Vec::new())
    }

    #[inline]
    fn allocate_atom(&mut self, atom: impl Into<String>) -> usize {
        self.frame.atoms.get_or_allocate(atom.into())
    }

    fn compile_statement<'b>(
        mut self,
        input: Statement<'a>,
        next_block: Option<usize>,
        break_stack: &'b BreakStack,
    ) -> Result<(Self, bool)> {
        let chunk = match input {
            Statement::Return(ReturnStatement { expression }) => {
                if let Some(expression) = expression {
                    self.compile(expression)?.append(Return)
                } else {
                    self.append(ReturnConstant {
                        constant: Constant::Undefined,
                    })
                }
            }
            Statement::Var(VarStatement { declarations }) => {
                let mut next = self;

                for VarDeclaration {
                    identifier,
                    expression,
                } in declarations
                {
                    let local = next.allocate_local(identifier, false);

                    if let Some(expression) = expression {
                        if !next.frame.locals.set_init_value(
                            local,
                            &expression,
                            &mut next.frame.atoms,
                        ) {
                            next = next.compile(expression)?.append(SetLocal { local });
                        }

                        if !next.options.module {
                            let name = next.allocate_atom(identifier);
                            next = next
                                .append(LoadEnvironmental {
                                    environmental: GlobalThis,
                                })
                                .append(GetLocal { local })
                                .append(SetNamed { name })
                        }
                    }
                }

                next
            }
            Statement::For(ForStatement::For {
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
                    let (next_builder, finalized) =
                        next.compile_statement(Statement::Var(vars), next_block, break_stack)?;

                    if finalized {
                        return Ok((next_builder, finalized));
                    } else {
                        next = next_builder
                    }
                } else if let Some(expression) = expression {
                    next = next.compile(expression)?;
                }

                next = next
                    .append(Jump {
                        to: condition_index,
                    })
                    .switch_to(condition_index);

                if let Some(condition) = condition {
                    next = next.compile(condition)?.append(CompareJump {
                        if_true: block_index,
                        if_false: next_index,
                    });
                } else {
                    next = next.append(Jump { to: block_index });
                }

                let next_break_stack = &break_stack.child(next_index, operation_index);
                next = next
                    .switch_to(block_index)
                    .compile_block(*block, operation_index, next_break_stack, |c| c)?
                    .append(Jump {
                        to: operation_index,
                    })
                    .switch_to(operation_index);

                if let Some(operation) = operation {
                    next = next.compile(operation)?
                }

                next.append(Jump {
                    to: condition_index,
                })
                .switch_to(next_index)
            }
            Statement::For(ForStatement::ForIn {
                allocate,
                identifier,
                expression,
                block,
            }) => {
                let input_str = format!("for (var {0}__iterator = true, {0}__iterator_i = 0, {0}; {0}__iterator_i < {0}__iterator.length; {0}__iterator_i ++) {{ {0} = {0}__iterator[{0}__iterator_i]; }}", identifier);
                let mut for_statement: ForStatement =
                    parse_input_as(&input_str, ParseContext::TOP).unwrap();

                if let ForStatement::For { block: b, vars, .. } = &mut for_statement {
                    if let Statement::Block(statement) = b.as_mut() {
                        statement.statements.push(DeclStatement::Statement(*block));
                    }

                    vars.as_mut().unwrap().declarations[0].expression = Some(Expression::Call {
                        expression: Box::new(Expression::Reference(Accessor {
                            expression: Box::new(Expression::Reference(Id("Object"))),
                            null_safe: false,
                            accessor: "keys",
                        })),
                        parameters: vec![expression],
                    });
                };

                self.compile_statement(Statement::For(for_statement), None, break_stack)?
                    .0

                // self.switch_to(next_index)
                // self
                // todo: Make this work
            }
            Statement::Expression(expression) => self.compile(expression)?,
            Statement::If(IfStatement {
                condition,
                true_block,
                false_block,
            }) => {
                let if_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                let else_index = if false_block.is_some() {
                    self.allocate_chunk()
                } else {
                    next_index
                };

                let mut next = self.compile(condition)?.append(CompareJump {
                    if_true: if_index,
                    if_false: else_index,
                });

                next = next.switch_to(if_index).compile_block(
                    *true_block,
                    next_index,
                    break_stack,
                    |c| c,
                )?;

                if let Some(else_block) = false_block {
                    next = next.switch_to(else_index).compile_block(
                        *else_block,
                        next_index,
                        break_stack,
                        |c| c,
                    )?
                };

                next.switch_to(next_index)
            }
            Statement::While(WhileStatement {
                condition,
                loop_block,
            }) => {
                let condition_index = self.allocate_chunk();
                let block_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                let next_break_stack = &break_stack.child(next_index, condition_index);

                self.append(Jump {
                    to: condition_index,
                })
                .switch_to(condition_index)
                .compile(condition)?
                .append(CompareJump {
                    if_true: block_index,
                    if_false: next_index,
                })
                .switch_to(block_index)
                .compile_block(*loop_block, condition_index, next_break_stack, |c| c)?
                .switch_to(next_index)
            }
            Statement::Try(TryStatement {
                try_block,
                finally_block,
                catch_block,
                catch_binding,
            }) => {
                let try_index = self.allocate_chunk();
                let catch_index = self.allocate_chunk();
                let finally_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                let mut next = self.append(Jump { to: try_index }).switch_to(try_index);

                let has_catch_block = catch_block.is_some();

                if has_catch_block {
                    next = next.append(Catch { chunk: catch_index });
                }

                next = next.compile_block(try_block, finally_index, break_stack, |c| {
                    if has_catch_block {
                        c.append(DropCatch { chunk: catch_index })
                    } else {
                        c
                    }
                })?;

                if let Some(catch_block) = catch_block {
                    next = next
                        .switch_to(catch_index)
                        .append(DropCatch { chunk: catch_index });

                    if let Some(binding) = catch_binding {
                        let binding = next.allocate_local(binding, false);
                        next = next.append(SetLocal { local: binding });
                    }

                    next = next.compile_block(catch_block, finally_index, break_stack, |c| c)?;
                }

                next = next.switch_to(finally_index).compile_block(
                    finally_block.unwrap_or(BlockStatement { statements: vec![] }),
                    next_index,
                    break_stack,
                    |c| c,
                )?;

                next.switch_to(next_index)
            }
            Statement::ThrowStatement(ThrowStatement { expression }) => {
                self.compile(expression)?.append(ThrowValue)
            }
            Statement::Block(block) => {
                self.compile_block(block, next_block.unwrap_or(usize::MAX), break_stack, |c| c)?
            }
            Statement::Break => self.append(Jump {
                to: break_stack.get_break()?,
            }),
            Statement::Continue => self.append(Jump {
                to: break_stack.get_continue()?,
            }),
        };

        Ok((chunk, false))
    }

    fn compile_decl_statement<'b>(
        mut self,
        input: DeclStatement<'a>,
        next_block: Option<usize>,
        break_stack: &'b BreakStack,
    ) -> Result<(Self, bool)> {
        let (chunk, finalised) = match input {
            DeclStatement::Class(class) => {
                let mut class_declaration = Class {
                    construct: None,
                    name: Some(0),
                    atoms: vec![class.name.to_owned()],
                    methods: vec![],
                    static_methods: vec![],
                    private_fields: 0,
                    properties: vec![],
                    static_properties: vec![],
                };

                for member in class.members.into_iter() {
                    match member {
                        ClassMember::Constructor(FunctionStatement {
                            statements,
                            identifier,
                            arguments,
                        }) => {
                            class_declaration.construct = Some(compile_function(
                                identifier,
                                self.frame.locals.child(),
                                arguments.to_owned(),
                                statements.statements.to_owned(),
                                DEFAULT_OPTIONS,
                            )?)
                        }

                        ClassMember::Function {
                            function:
                                FunctionStatement {
                                    statements,
                                    identifier,
                                    arguments,
                                },
                            is_static: false,
                        } => class_declaration.methods.push(compile_function(
                            identifier,
                            self.frame.locals.child(),
                            arguments.to_owned(),
                            statements.statements.to_owned(),
                            DEFAULT_OPTIONS,
                        )?),

                        ClassMember::Function {
                            function:
                                FunctionStatement {
                                    statements,
                                    identifier,
                                    arguments,
                                },
                            is_static: true,
                        } => class_declaration.static_methods.push(compile_function(
                            identifier,
                            self.frame.locals.child(),
                            arguments.to_owned(),
                            statements.statements.to_owned(),
                            DEFAULT_OPTIONS,
                        )?),

                        ClassMember::PrivateField {
                            field: Field { identifier },
                            ..
                        } => {}

                        ClassMember::Getter {
                            function,
                            is_static,
                        } => {
                            let declarations = if is_static {
                                &mut class_declaration.static_properties
                            } else {
                                &mut class_declaration.properties
                            };

                            let compiled_function = compile_function(
                                function.identifier,
                                self.frame.locals.child(),
                                function.arguments.to_owned(),
                                function.statements.statements.to_owned(),
                                DEFAULT_OPTIONS,
                            )?;

                            if let Some(existing) = declarations
                                .iter_mut()
                                .find(|v| v.name == function.identifier)
                            {
                                existing.getter = Some(compiled_function);
                            } else {
                                declarations.push(Property {
                                    name: function.identifier.to_owned(),
                                    getter: Some(compiled_function),
                                    setter: None,
                                })
                            }
                        }

                        ClassMember::Setter {
                            function,
                            is_static,
                        } => {
                            let declarations = if is_static {
                                &mut class_declaration.static_properties
                            } else {
                                &mut class_declaration.properties
                            };

                            let compiled_function = compile_function(
                                function.identifier,
                                self.frame.locals.child(),
                                function.arguments.to_owned(),
                                function.statements.statements.to_owned(),
                                DEFAULT_OPTIONS,
                            )?;

                            if let Some(existing) = declarations
                                .iter_mut()
                                .find(|v| v.name == function.identifier)
                            {
                                existing.setter = Some(compiled_function);
                            } else {
                                declarations.push(Property {
                                    name: function.identifier.to_owned(),
                                    setter: Some(compiled_function),
                                    getter: None,
                                })
                            }
                        }
                    }
                }

                let class_id = self.frame.classes.allocate(class_declaration);

                let identifier = self.allocate_local(class.name, false);

                let mut next = self;

                let extends = if let Some(extends) = class.extends {
                    next = next.compile(extends)?;
                    true
                } else {
                    false
                };

                let n = next
                    .append(GetClass {
                        class: class_id,
                        extends,
                    })
                    .append(SetLocal { local: identifier });

                (n, false)
            }
            DeclStatement::Function(FunctionStatement {
                identifier,
                arguments,
                statements: BlockStatement { statements },
            }) => {
                let local = self.allocate_local(identifier, false);
                let function = self.frame.functions.allocate(compile_function(
                    identifier,
                    self.frame.locals.child(),
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?);

                let mut next = self;

                if !next.frame.locals.set_init_function(local, function) {
                    next = next
                        .append(GetFunction { function })
                        .append(SetLocal { local });
                }

                if !next.options.module {
                    let name = next.allocate_atom(identifier);
                    next = next
                        .append(LoadEnvironmental {
                            environmental: GlobalThis,
                        })
                        .append(GetLocal { local })
                        .append(SetNamed { name });
                }

                (next, false)
            }
            DeclStatement::Statement(statement) => {
                self.compile_statement(statement, next_block, break_stack)?
            }
            DeclStatement::Const(ConstStatement { declarations }) => {
                let mut next = self;

                for VarDeclaration {
                    identifier,
                    expression,
                } in declarations
                {
                    let local = next.allocate_local(identifier, false);

                    if let Some(expression) = expression {
                        if !next.frame.locals.set_init_value(
                            local,
                            &expression,
                            &mut next.frame.atoms,
                        ) {
                            next = next.compile(expression)?.append(SetLocal { local });
                        }

                        if !next.options.module {
                            let name = next.allocate_atom(identifier);
                            next = next
                                .append(LoadEnvironmental {
                                    environmental: GlobalThis,
                                })
                                .append(GetLocal { local })
                                .append(SetNamed { name })
                        }
                    }
                }

                (next, false)
            }
        };

        Ok((chunk, finalised))
    }

    fn compile_block<'b, B: Into<BlockStatement<'a>>>(
        self,
        statements: B,
        next_block: usize,
        break_stack: &'b BreakStack,
        finalize: impl Fn(Self) -> Self,
    ) -> Result<Self> {
        let mut next = self;
        for statement in statements.into().statements.into_iter() {
            let (next_builder, finalised) =
                next.compile_decl_statement(statement, Some(next_block), break_stack)?;

            next = next_builder;

            if finalised {
                next = finalize(next);
                break;
            }
        }

        next = finalize(next);
        next = next.append(Jump { to: next_block });

        Ok(next)
    }

    fn finalize(self) -> Frame {
        self.frame
    }
}

const DEFAULT_OPTIONS: CompilerOptions = CompilerOptions { module: true };

fn compile_function<'a>(
    name: &'a str,
    parent: LocalAllocator,
    arguments: Vec<&'a str>,
    statements: Vec<DeclStatement<'a>>,
    options: CompilerOptions,
) -> Result<Function> {
    let mut frame = Frame {
        atoms: vec![],
        chunks: vec![],
        functions: vec![],
        locals: parent.child(),
        classes: vec![],
    };

    frame.locals.allocate_identifier("arguments", false);

    for argument in arguments {
        frame.locals.allocate_identifier(argument, true);
    }

    let mut chunk = frame.new_chunk(options);
    for statement in statements.into_iter() {
        chunk = chunk
            .compile_decl_statement(statement, None, &BreakStack::new())?
            .0;
    }

    chunk = chunk.append(ReturnConstant {
        constant: Constant::Undefined,
    });

    let frame = chunk.finalize();

    let allocator = frame.locals;
    let args_size = allocator.args_len();
    let local_size = allocator.len();
    let locals_init = allocator.get_init();

    let mut locals = vec![];
    #[cfg(feature = "eval")]
    {
        locals = allocator.locals;
    }

    Ok(Function {
        locals,
        atoms: frame.atoms,
        classes: frame.classes,
        instructions: freeze_instructions(frame.chunks),
        functions: frame.functions,
        args_size,
        locals_init,
        stack_size: 0,
        name: Some(name.to_owned()),
        local_size,
    })
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

#[cfg(feature = "eval")]
pub fn compile_eval(locals: Vec<Local>, input: ParsedModule) -> Result<Function> {
    let mut frame = Frame {
        atoms: vec![],
        chunks: vec![],
        functions: vec![],
        classes: vec![],
        locals: LocalAllocator {
            locals,
            current_id: 0,
            init: vec![],
        }
        .child(),
    };

    let return_value = matches!(
        input.block.last(),
        Some(DeclStatement::Statement(Statement::Expression(_)))
    );

    frame.locals.allocate_identifier("arguments", false);

    let mut chunk = frame.new_chunk(CompilerOptions { module: true });
    for statement in input.block.into_iter() {
        chunk = chunk
            .compile_decl_statement(statement, None, &BreakStack::new())?
            .0;
    }

    if return_value {
        chunk = chunk.append(Return);
    } else {
        chunk = chunk.append(ReturnConstant {
            constant: Undefined,
        })
    }

    let finalised = chunk.finalize();

    let locals_len = finalised.locals.len();
    let args_size = finalised.locals.args_len();
    let locals_init = finalised.locals.get_init();

    Ok(Function {
        locals: finalised.locals.locals,
        functions: finalised.functions.into_iter().map(From::from).collect(),
        instructions: freeze_instructions(finalised.chunks),
        local_size: locals_len,
        atoms: finalised.atoms,
        stack_size: 0,
        name: Some("eval".to_owned()),
        locals_init,
        args_size,
        classes: finalised.classes,
    })
}

pub fn compile<'a>(
    id: &'a str,
    input: ParsedModule<'a>,
    options: CompilerOptions,
) -> Result<Module> {
    debug!("{:#?}", input);

    Ok(Module {
        init: compile_function(
            id,
            LocalAllocator::new_root(),
            Vec::new(),
            input.block,
            options,
        )?,
    })
}
