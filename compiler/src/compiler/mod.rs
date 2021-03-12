use crate::parser::ast::{
    BinaryOperator, BlockStatement, Expression, ForStatement, FunctionStatement, IfStatement,
    ParsedModule, Reference, ReturnStatement, Statement, ThrowStatement, TryStatement,
    UnaryOperator, VarDeclaration, VarStatement, WhileStatement,
};

use crate::result::{CompilerError, InternalError, Result, SyntaxError};
use instruction_set::Constant::Undefined;
use instruction_set::Environmental::GlobalThis;
use instruction_set::Instruction::*;
use instruction_set::{
    Chunk, Constant, Environmental, Function, Instruction, Local, LocalInit, Module,
};
use log::debug;

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
struct Frame {
    chunks: Vec<Chunk>,
    functions: Vec<Function>,
    atoms: Vec<String>,
    locals: LocalAllocator,
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
    fn append(mut self, instr: Instruction) -> Self {
        self.frame.chunks[self.chunk_index].push(instr);

        self
    }

    fn allocate_local(&mut self, id: &str, argument: bool) -> usize {
        let existing_position = self
            .frame
            .locals
            .locals
            .iter()
            .rev()
            .position(|Local { name, frame, .. }| name == id && *frame == 0);

        if let Some(existing) = existing_position {
            existing
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

enum Resolution {
    Resolved { local: usize },
    Capture { frame: usize, local: usize },
    Unresolved,
}

#[derive(Debug)]
struct LocalAllocator {
    locals: Vec<Local>,
    init: Vec<Option<LocalInit>>,
    current_id: usize,
}

impl LocalAllocator {
    fn resolve_identifier(&self, id: &str) -> Resolution {
        let result = self
            .locals
            .iter()
            .rev()
            .find(|Local { name, .. }| name == id);

        match result {
            Some(Local {
                local, frame: 0, ..
            }) => Resolution::Resolved { local: *local },
            Some(Local { local, frame, .. }) => Resolution::Capture {
                local: *local,
                frame: *frame,
            },
            None => Resolution::Unresolved,
        }
    }

    fn allocate_identifier(&mut self, id: impl Into<String>, argument: bool) -> usize {
        let local = self.current_id;
        self.current_id += 1;

        self.locals.push(Local {
            name: id.into(),
            local,
            frame: 0,
            argument,
        });

        self.init.push(None);

        local
    }

    fn new_root() -> Self {
        LocalAllocator {
            current_id: 0,
            locals: vec![],
            init: vec![],
        }
    }

    fn child(&self) -> Self {
        let locals: Vec<Local> = self
            .locals
            .iter()
            .map(
                |Local {
                     name,
                     local,
                     frame,
                     argument,
                 }| Local {
                    name: name.to_owned(),
                    local: *local,
                    frame: frame + 1,
                    argument: *argument,
                },
            )
            .collect();

        LocalAllocator {
            locals,
            current_id: 0,
            init: vec![],
        }
    }

    fn len(&self) -> usize {
        self.locals
            .iter()
            .filter(|Local { frame, .. }| *frame == 0)
            .count()
    }

    fn args_len(&self) -> usize {
        self.locals
            .iter()
            .filter(
                |Local {
                     frame, argument, ..
                 }| *frame == 0 && *argument,
            )
            .count()
    }

    fn set_init_value(&mut self, local: usize, expression: &Expression) -> bool {
        let constant = if let Some(constant) = expression.constant() {
            constant
        } else {
            return false;
        };

        if self.init.get(local).unwrap_or(&None).is_none() {
            self.init[local] = Some(LocalInit::Constant(constant));

            true
        } else {
            false
        }
    }

    fn set_init_function(&mut self, local: usize, function_id: usize) -> bool {
        if self.init.get(local).is_none() {
            self.init[local] = Some(LocalInit::Function(function_id));
            true
        } else {
            false
        }
    }

    fn get_init(&self) -> Vec<LocalInit> {
        self.init
            .iter()
            .map(|v| v.clone().unwrap_or_default())
            .collect()
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

impl BinaryOperator {
    fn to_op(&self) -> Instruction {
        match self {
            BinaryOperator::Add => Add,
            BinaryOperator::Sub => Subtract,
            BinaryOperator::Mul => Multiply,
            BinaryOperator::Div => Divide,
            BinaryOperator::Mod => Modulo,
            BinaryOperator::GreaterThan => GreaterThan,
            BinaryOperator::GreaterThanEqual => GreaterThanEqual,
            BinaryOperator::LessThan => LessThan,
            BinaryOperator::LessThanEqual => LessThanEqual,
            BinaryOperator::NotEqualTo => NotEqualTo,
            BinaryOperator::EqualTo => EqualTo,
            BinaryOperator::StrictEqualTo => StrictEqualTo,
            BinaryOperator::NotStrictEqualTo => NotStrictEqualTo,
            BinaryOperator::LeftShift => LeftShift,
            BinaryOperator::RightShift => RightShift,
            BinaryOperator::RightShiftUnsigned => RightShiftUnsigned,
            BinaryOperator::InstanceOf => InstanceOf,
            BinaryOperator::LogicalOr => panic!("Lor is handled separately"),
            BinaryOperator::LogicalAnd => panic!("Land is handled separately"),
        }
    }
}

impl UnaryOperator {
    fn to_op(&self) -> Instruction {
        match self {
            UnaryOperator::TypeOf => TypeOf,
            UnaryOperator::LogicalNot => LogicalNot,
            UnaryOperator::Sub => Neg,
            UnaryOperator::Add => todo!("Not implemented"),
            UnaryOperator::Delete => Delete,
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
        let atom = atom.into();

        self.frame.atoms.allocate(atom)
    }

    fn compile_expression(mut self, input: Expression<'a>) -> Result<Self> {
        let chunk = match input {
            Expression::BinaryExpression {
                left,
                right,
                operator: BinaryOperator::LogicalOr,
            } => {
                let right_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                self.compile_expression(*left)?
                    .append(LogicalOr {
                        left: next_index,
                        right: right_index,
                    })
                    .switch_to(right_index)
                    .compile_expression(*right)?
                    .append(Jump { to: next_index })
                    .switch_to(next_index)
            }
            Expression::BinaryExpression {
                left,
                right,
                operator: BinaryOperator::LogicalAnd,
            } => {
                let right_index = self.allocate_chunk();
                let next_index = self.allocate_chunk();

                self.compile_expression(*left)?
                    .append(LogicalAnd {
                        left: next_index,
                        right: right_index,
                    })
                    .switch_to(right_index)
                    .compile_expression(*right)?
                    .append(Jump { to: next_index })
                    .switch_to(next_index)
            }
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
            } => internal_error!("Add unary operator not implemented"),
            Expression::UnaryExpression { value, operator } => {
                self.compile_expression(*value)?.append(operator.to_op())
            }
            Expression::Float(value) => self.append(LoadConstant {
                constant: Constant::Float(value),
            }),
            Expression::Boolean(value) => self.append(LoadConstant {
                constant: Constant::Boolean(value),
            }),
            Expression::String(value) => self.append(LoadConstant {
                constant: Constant::String(value),
            }),
            Expression::Inc { reference, .. } => self
                .compile_expression(Expression::Reference(reference))?
                .append(Increment { by: 1.0 }),
            Expression::Dec { reference, .. } => self
                .compile_expression(Expression::Reference(reference))?
                .append(Increment { by: -1.0 }),
            Expression::Null => self.append(LoadConstant {
                constant: Constant::Null,
            }),
            Expression::Assign {
                assign_to: Reference::Id(id),
                expression,
            } => {
                let mut next = self.compile_expression(*expression)?;

                match next.resolve_identifier(id) {
                    Resolution::Resolved { local } => next.append(SetLocal { local }),
                    Resolution::Capture { frame, local } => {
                        next.append(SetCapture { local, frame })
                    }
                    Resolution::Unresolved => {
                        let name = next.allocate_atom(id);
                        next.append(LoadEnvironmental {
                            environmental: GlobalThis,
                        })
                        .append(SetNamed { name })
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
            } => {
                let name = self.allocate_atom(accessor);
                self.compile_expression(*target)?
                    .compile_expression(*expression)?
                    .append(SetNamed { name })
            }
            Expression::Assign {
                assign_to:
                    Reference::ComputedAccessor {
                        accessor,
                        expression: target,
                        ..
                    },
                expression,
            } => self
                .compile_expression(*target)?
                .compile_expression(*accessor)?
                .compile_expression(*expression)?
                .append(Set),
            Expression::Call {
                expression,
                parameters,
            } => {
                let args_count = parameters.len();

                parameters
                    .into_iter()
                    .try_fold(self, |next, expression| {
                        next.compile_expression(expression)
                            .map(|n| n.append(Resolve))
                    })?
                    .compile_expression(*expression)?
                    .append(Call { args_count })
            }

            Expression::NewWithArgs { target, parameters } => {
                // TODO ME

                let args_count = parameters.len();

                parameters
                    .into_iter()
                    .try_fold(self, |next, expression| {
                        next.compile_expression(expression)
                            .map(|n| n.append(Resolve))
                    })?
                    .compile_expression(*target)?
                    .append(CallNew { args_count })
            }
            Expression::ObjectLiteral { attributes } => {
                let next = self.append(LoadEnvironmental {
                    environmental: Environmental::NewObject,
                });

                attributes
                    .into_iter()
                    .try_fold(next, |mut builder, (attr, expression)| {
                        let attr_id = builder.frame.atoms.allocate(attr);

                        builder
                            .append(Duplicate)
                            .compile_expression(expression)
                            .map(|builder| builder.append(SetNamed { name: attr_id }))
                    })?
            }
            Expression::ArrayLiteral { attributes } => {
                let mut next = self;

                let attribute_count = attributes.len();
                next = attributes
                    .into_iter()
                    .try_fold(next, |builder, expression| {
                        builder.compile_expression(expression)
                    })?;

                let name = next.allocate_atom("Array");

                next.append(LoadEnvironmental {
                    environmental: GlobalThis,
                })
                .append(GetNamed { name })
                .append(CallNew {
                    args_count: attribute_count,
                })
            }
            Expression::ConditionalOperator {
                condition,
                if_true,
                if_false,
            } => {
                let mut next = self;

                let if_true_chunk = next.allocate_chunk();
                let if_false_chunk = next.allocate_chunk();
                let next_chunk = next.allocate_chunk();

                next.compile_expression(*condition)?
                    .append(CompareJump {
                        if_true: if_true_chunk,
                        if_false: if_false_chunk,
                    })
                    .switch_to(if_true_chunk)
                    .compile_expression(*if_true)?
                    .append(Jump { to: next_chunk })
                    .switch_to(if_false_chunk)
                    .compile_expression(*if_false)?
                    .append(Jump { to: next_chunk })
                    .switch_to(next_chunk)
            }
            Expression::Reference(Reference::This) => self.append(LoadEnvironmental {
                environmental: Environmental::This,
            }),
            Expression::Reference(Reference::Id(id)) => match self.resolve_identifier(id) {
                Resolution::Resolved { local } => self.append(GetLocal { local }),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => self.append(GetCapture {
                    frame: capture_frame,
                    local,
                }),
                Resolution::Unresolved => {
                    let name = self.allocate_atom(id);
                    self.append(LoadEnvironmental {
                        environmental: GlobalThis,
                    })
                    .append(GetNamed { name })
                }
            },
            Expression::Reference(Reference::Accessor {
                expression,
                accessor,
                null_safe,
            }) => {
                let mut next = self.compile_expression(*expression)?;

                if null_safe {
                    todo!("No support for null safe get")
                } else {
                    let name = next.allocate_atom(accessor);

                    next.append(GetNamed { name })
                }
            }
            Expression::Reference(Reference::ComputedAccessor {
                expression,
                accessor,
                null_safe,
            }) => {
                let next = self
                    .compile_expression(*expression)?
                    .compile_expression(*accessor)?;

                if null_safe {
                    todo!("No support for null safe get")
                } else {
                    next.append(Get)
                }
            }
            Expression::Function {
                name: identifier,
                statements: BlockStatement { statements },
                arguments,
            } => {
                let identifier = identifier.unwrap_or("(anonymous)");
                let function = self.frame.functions.allocate(Function {
                    instructions: vec![],
                    atoms: vec![],
                    stack_size: 0,
                    functions: vec![],
                    name: Some(identifier.to_owned()),
                    locals: vec![],
                    local_size: 0,
                    args_size: 0,
                    locals_init: vec![],
                });

                let allocator = self.frame.locals.child();
                let mut next = self.append(GetFunction { function });

                let frame = &mut next.frame;

                let Frame {
                    chunks,
                    functions,
                    locals,
                    atoms,
                } = compile_function(
                    identifier,
                    allocator,
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?;

                frame.functions[function].instructions = freeze_instructions(chunks);
                frame.functions[function].functions = functions.into_iter().collect();
                frame.functions[function].atoms = atoms;
                frame.functions[function].local_size = locals.len();
                frame.functions[function].args_size = locals.args_len();
                frame.functions[function].locals_init = locals.get_init();

                #[cfg(feature = "eval")]
                {
                    frame.functions[function].locals = locals.locals;
                }

                next
            }
            Expression::Undefined => self.append(LoadConstant {
                constant: Constant::Undefined,
            }),
            node => {
                internal_error!("Unexpected expression {:?}", node);
            }
        };

        Ok(chunk)
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
                    self.compile_expression(expression)?.append(Return)
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
                        if !next.frame.locals.set_init_value(local, &expression) {
                            next = next
                                .compile_expression(expression)?
                                .append(SetLocal { local });
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
                    next = next.compile_expression(expression)?;
                }

                next = next
                    .append(Jump {
                        to: condition_index,
                    })
                    .switch_to(condition_index);

                if let Some(condition) = condition {
                    next = next.compile_expression(condition)?.append(CompareJump {
                        if_true: block_index,
                        if_false: next_index,
                    });
                } else {
                    next = next.append(Jump { to: block_index });
                }

                let next_break_stack = &break_stack.child(next_index, condition_index);
                next = next
                    .switch_to(block_index)
                    .compile_block(block, operation_index, next_break_stack, |c| c)?
                    .append(Jump {
                        to: operation_index,
                    })
                    .switch_to(operation_index);

                if let Some(operation) = operation {
                    next = next.compile_expression(operation)?
                }

                next.append(Jump {
                    to: condition_index,
                })
                .switch_to(next_index)
            }
            Statement::For(ForStatement::ForIn { .. }) => {
                self
                // todo: Make this work
            }
            Statement::Function(FunctionStatement {
                identifier,
                arguments,
                statements: BlockStatement { statements },
            }) => {
                let local = self.allocate_local(identifier, false);
                let function = self.frame.functions.allocate(Function {
                    instructions: vec![],
                    stack_size: 0,
                    local_size: 0,
                    args_size: 0,
                    functions: vec![],
                    locals: vec![],
                    atoms: vec![],
                    name: Some(identifier.to_owned()),
                    locals_init: vec![],
                });

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

                let allocator = next.frame.locals.child();
                let frame = &mut next.frame;

                let Frame {
                    chunks,
                    functions,
                    atoms,
                    locals,
                } = compile_function(
                    identifier,
                    allocator,
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?;

                frame.functions[function].instructions = freeze_instructions(chunks);
                frame.functions[function].local_size = locals.len();
                frame.functions[function].functions =
                    functions.into_iter().map(From::from).collect();
                frame.functions[function].atoms = atoms;
                frame.functions[function].locals_init = locals.get_init();
                frame.functions[function].args_size = locals.args_len();

                #[cfg(feature = "eval")]
                {
                    frame.functions[function].locals = locals.locals;
                }

                next
            }
            Statement::Expression(expression) => self.compile_expression(expression)?,
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

                let mut next = self.compile_expression(condition)?.append(CompareJump {
                    if_true: if_index,
                    if_false: else_index,
                });

                next = next.switch_to(if_index).compile_block(
                    true_block,
                    next_index,
                    break_stack,
                    |c| c,
                )?;

                if let Some(else_block) = false_block {
                    next = next.switch_to(else_index).compile_block(
                        else_block,
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
                .compile_expression(condition)?
                .append(CompareJump {
                    if_true: block_index,
                    if_false: next_index,
                })
                .switch_to(block_index)
                .compile_block(loop_block, condition_index, next_break_stack, |c| c)?
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
                self.compile_expression(expression)?.append(ThrowValue)
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
            Statement::Class(_) => syntax_error!("Class statements not supported"),
        };

        Ok((chunk, false))
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
                next.compile_statement(statement, Some(next_block), break_stack)?;

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
    _name: &'a str,
    parent: LocalAllocator,
    arguments: Vec<&'a str>,
    statements: Vec<Statement<'a>>,
    options: CompilerOptions,
) -> Result<Frame> {
    let mut frame = Frame {
        atoms: vec![],
        chunks: vec![],
        functions: vec![],
        locals: parent.child(),
    };

    frame.locals.allocate_identifier("arguments", false);

    for argument in arguments {
        frame.locals.allocate_identifier(argument, true);
    }

    let mut chunk = frame.new_chunk(options);
    for statement in statements.into_iter() {
        chunk = chunk
            .compile_statement(statement, None, &BreakStack::new())?
            .0;
    }

    chunk = chunk.append(ReturnConstant {
        constant: Constant::Undefined,
    });

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

#[cfg(feature = "eval")]
pub fn compile_eval(locals: Vec<Local>, input: ParsedModule) -> Result<Function> {
    let mut frame = Frame {
        atoms: vec![],
        chunks: vec![],
        functions: vec![],
        locals: LocalAllocator {
            locals,
            current_id: 0,
            init: vec![],
        }
        .child(),
    };

    let return_value = if let Some(Statement::Expression(_)) = input.block.last() {
        true
    } else {
        false
    };

    frame.locals.allocate_identifier("arguments", false);

    let mut chunk = frame.new_chunk(CompilerOptions { module: true });
    for statement in input.block.into_iter() {
        chunk = chunk
            .compile_statement(statement, None, &BreakStack::new())?
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
    })
}

pub fn compile<'a>(
    id: &'a str,
    input: ParsedModule<'a>,
    options: CompilerOptions,
) -> Result<Module> {
    debug!("{:#?}", input);

    let frame = compile_function(
        id,
        LocalAllocator::new_root(),
        Vec::new(),
        input.block,
        options,
    )?;

    let local_size = frame.locals.len();
    let locals_init = frame.locals.get_init();
    let args_size = frame.locals.args_len();

    #[allow(unused_assignments)]
    let mut locals = vec![];
    #[cfg(feature = "eval")]
    {
        locals = frame.locals.locals;
    }

    Ok(Module {
        init: Function {
            instructions: freeze_instructions(frame.chunks),
            stack_size: 0,
            local_size,
            functions: frame.functions.into_iter().map(From::from).collect(),
            name: Some("(anonymous)".to_owned()),
            atoms: frame.atoms,
            locals,
            locals_init,
            args_size,
        },
    })
}
