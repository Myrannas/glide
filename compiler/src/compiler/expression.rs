use crate::compiler::locals::Resolution;
use crate::compiler::{compile_function, Bucket, ChunkBuilder, Compile, DEFAULT_OPTIONS};
use crate::internal_error;
use crate::parser::ast::{BinaryOperator, BlockStatement, Expression, Reference, UnaryOperator};
use crate::result::InternalError;
use crate::result::{CompilerError, Result};
use instruction_set::Environmental::GlobalThis;
use instruction_set::Instruction::{
    Call, CallNew, CompareJump, Duplicate, Get, GetCapture, GetFunction, GetLocal, GetNamed,
    Increment, Jump, LoadConstant, LoadEnvironmental, LogicalAnd, LogicalOr, Resolve, Set,
    SetCapture, SetLocal, SetNamed,
};
use instruction_set::{Constant, Environmental, Instruction};

impl<'a> Compile for Expression<'a> {
    fn compile(self, mut builder: ChunkBuilder) -> Result<ChunkBuilder> {
        let chunk = match self {
            Expression::BinaryExpression {
                left,
                right,
                operator: BinaryOperator::LogicalOr,
            } => {
                let right_index = builder.allocate_chunk();
                let next_index = builder.allocate_chunk();

                builder
                    .compile(*left)?
                    .append(LogicalOr {
                        left: next_index,
                        right: right_index,
                    })
                    .switch_to(right_index)
                    .compile(*right)?
                    .append(Jump { to: next_index })
                    .switch_to(next_index)
            }
            Expression::BinaryExpression {
                left,
                right,
                operator: BinaryOperator::LogicalAnd,
            } => {
                let right_index = builder.allocate_chunk();
                let next_index = builder.allocate_chunk();

                builder
                    .compile(*left)?
                    .append(LogicalAnd {
                        left: next_index,
                        right: right_index,
                    })
                    .switch_to(right_index)
                    .compile(*right)?
                    .append(Jump { to: next_index })
                    .switch_to(next_index)
            }
            Expression::BinaryExpression {
                left,
                right,
                operator,
            } => builder.compile(*right)?.compile(*left)?.compile(operator)?,
            Expression::UnaryExpression {
                operator: UnaryOperator::Add,
                value,
            } => builder.compile(*value)?,
            Expression::UnaryExpression { value, operator } => {
                builder.compile(*value)?.compile(operator)?
            }
            Expression::Float(value) => builder.append(LoadConstant {
                constant: Constant::Float(value),
            }),
            Expression::Boolean(value) => builder.append(LoadConstant {
                constant: Constant::Boolean(value),
            }),
            Expression::String(value) => {
                let constant = builder.frame.atoms.allocate(value);
                builder.append(LoadConstant {
                    constant: Constant::Atom(constant),
                })
            }
            Expression::Inc { reference, .. } => builder
                .compile(Expression::Reference(reference))?
                .append(Increment {
                    by: 1.0,
                    pre: false,
                }),
            Expression::Dec { reference, .. } => builder
                .compile(Expression::Reference(reference))?
                .append(Increment {
                    by: -1.0,
                    pre: false,
                }),
            Expression::Null => builder.append(LoadConstant {
                constant: Constant::Null,
            }),
            Expression::Assign {
                assign_to: Reference::Id(id),
                expression,
            } => {
                let mut next = builder.compile(*expression)?;

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
                let name = builder.allocate_atom(accessor);
                builder
                    .compile(*target)?
                    .compile(*expression)?
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
            } => builder
                .compile(*target)?
                .compile(*accessor)?
                .compile(*expression)?
                .append(Set),
            Expression::Call {
                expression,
                parameters,
            } => {
                let args_count = parameters.len();

                parameters
                    .into_iter()
                    .try_fold(builder, |next, expression| {
                        next.compile(expression).map(|n| n.append(Resolve))
                    })?
                    .compile(*expression)?
                    .append(Call { args_count })
            }

            Expression::NewWithArgs { target, parameters } => {
                // TODO ME

                let args_count = parameters.len();

                parameters
                    .into_iter()
                    .try_fold(builder, |next, expression| {
                        next.compile(expression).map(|n| n.append(Resolve))
                    })?
                    .compile(*target)?
                    .append(CallNew { args_count })
            }
            Expression::ObjectLiteral { attributes } => {
                let next = builder.append(LoadEnvironmental {
                    environmental: Environmental::NewObject,
                });

                attributes
                    .into_iter()
                    .try_fold(next, |mut builder, (attr, expression)| {
                        let attr_id = builder.frame.atoms.allocate(attr);

                        builder
                            .append(Duplicate)
                            .compile(expression)
                            .map(|builder| builder.append(SetNamed { name: attr_id }))
                    })?
            }
            Expression::ArrayLiteral { attributes } => {
                let mut next = builder;

                let attribute_count = attributes.len();
                next = attributes
                    .into_iter()
                    .try_fold(next, |builder, expression| builder.compile(expression))?;

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
                let mut next = builder;

                let if_true_chunk = next.allocate_chunk();
                let if_false_chunk = next.allocate_chunk();
                let next_chunk = next.allocate_chunk();

                next.compile(*condition)?
                    .append(CompareJump {
                        if_true: if_true_chunk,
                        if_false: if_false_chunk,
                    })
                    .switch_to(if_true_chunk)
                    .compile(*if_true)?
                    .append(Jump { to: next_chunk })
                    .switch_to(if_false_chunk)
                    .compile(*if_false)?
                    .append(Jump { to: next_chunk })
                    .switch_to(next_chunk)
            }
            Expression::Reference(Reference::This) => builder.append(LoadEnvironmental {
                environmental: Environmental::This,
            }),
            Expression::Reference(Reference::Id(id)) => match builder.resolve_identifier(id) {
                Resolution::Resolved { local } => builder.append(GetLocal { local }),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => builder.append(GetCapture {
                    frame: capture_frame,
                    local,
                }),
                Resolution::Unresolved => {
                    let name = builder.allocate_atom(id);
                    builder
                        .append(LoadEnvironmental {
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
                let mut next = builder.compile(*expression)?;

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
                let next = builder.compile(*expression)?.compile(*accessor)?;

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
                let function = builder.frame.functions.allocate(compile_function(
                    identifier,
                    builder.frame.locals.child(),
                    arguments,
                    statements,
                    DEFAULT_OPTIONS,
                )?);

                let mut next = builder.append(GetFunction { function });

                next
            }
            Expression::Undefined => builder.append(LoadConstant {
                constant: Constant::Undefined,
            }),
            Expression::Add { expressions } => {
                let mut next = builder;

                let times = expressions.len() as u8 - 1;

                for expression in expressions.into_iter().rev() {
                    next = next.compile(expression)?;
                }

                next.append(Instruction::Add { times })
            }
            node => {
                internal_error!("Unexpected expression {:?}", node);
            }
        };

        Ok(chunk)
    }
}
