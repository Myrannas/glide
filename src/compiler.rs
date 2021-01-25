use crate::ops::{add, bind, call, div, load, mul, ret, sub, truncate, Instruction};
use crate::parser::{Ast, ParsedModule};
use crate::value::StaticValue;
use crate::vm::{Function, Module};
use anyhow::{bail, Context, Result};

struct Frame<'a> {
    instructions: Vec<Instruction>,
    locals: Vec<&'a str>,
    parent: Option<&'a Frame<'a>>,
    functions: Vec<Function>,
}

enum Resolution {
    Resolved { local: usize },
    Capture { frame: usize, local: usize },
    Unresolved,
}

impl<'a> Frame<'a> {
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
}

fn compile_expression<'a>(input: Ast<'a>, frame: &mut Frame<'a>) -> Result<()> {
    match input {
        Ast::Add(left, right) => {
            compile_expression(*left, frame)?;
            compile_expression(*right, frame)?;
            frame.instructions.push(Instruction {
                constant: None,
                instr: add,
            })
        }
        Ast::Sub(left, right) => {
            compile_expression(*left, frame)?;
            compile_expression(*right, frame)?;
            frame.instructions.push(Instruction {
                constant: None,
                instr: sub,
            })
        }
        Ast::Mul(left, right) => {
            compile_expression(*left, frame)?;
            compile_expression(*right, frame)?;
            frame.instructions.push(Instruction {
                constant: None,
                instr: mul,
            })
        }
        Ast::Div(left, right) => {
            compile_expression(*left, frame)?;
            compile_expression(*right, frame)?;
            frame.instructions.push(Instruction {
                constant: None,
                instr: div,
            })
        }
        Ast::Float(value) => frame.instructions.push(Instruction {
            constant: Some(StaticValue::Float(value)),
            instr: load,
        }),
        Ast::Boolean(value) => frame.instructions.push(Instruction {
            constant: Some(StaticValue::Boolean(value)),
            instr: load,
        }),
        Ast::Return(expression) => {
            compile_expression(*expression, frame)?;
            frame.instructions.push(Instruction {
                constant: None,
                instr: ret,
            })
        }
        Ast::Var(id, expression) => {
            let idx = frame.locals.len();
            frame.locals.push(id);
            compile_expression(*expression, frame)?;
            frame.instructions.push(Instruction {
                constant: Some(StaticValue::Local(idx)),
                instr: bind,
            })
        }
        Ast::Id(id) => {
            match frame.resolve_identifier(id) {
                Resolution::Resolved { local } => frame.instructions.push(Instruction {
                    constant: Some(StaticValue::Local(local)),
                    instr: load,
                }),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => {
                    frame.instructions.push(Instruction {
                        constant: Some(StaticValue::Capture(capture_frame, local)),
                        instr: load,
                    });
                }
                Resolution::Unresolved => {
                    bail!("Reference unresolved {}", id)
                }
            };
        }
        Ast::Function(id, args, block) => {
            frame.locals.push(id);

            frame.instructions.push(Instruction {
                instr: load,
                constant: Some(StaticValue::Function(frame.functions.len())),
            });

            frame.instructions.push(Instruction {
                instr: bind,
                constant: Some(StaticValue::Local(frame.locals.len() - 1)),
            });

            let inner_frame = compile_function(id, Some(frame), args, block)?;

            frame.functions.push(Function {
                instructions: inner_frame.instructions,
                stack_size: 0,
                local_size: inner_frame.locals.len(),
                functions: inner_frame.functions,
                name: id.to_owned(),
            });
        }
        Ast::Call(id, _arguments) => {
            match frame.resolve_identifier(id) {
                Resolution::Resolved { local } => frame.instructions.push(Instruction {
                    constant: Some(StaticValue::Local(local)),
                    instr: load,
                }),
                Resolution::Capture {
                    local,
                    frame: capture_frame,
                } => {
                    frame.instructions.push(Instruction {
                        constant: Some(StaticValue::Capture(capture_frame, local)),
                        instr: load,
                    });
                }
                Resolution::Unresolved => {
                    bail!("Reference unresolved {}", id)
                }
            };

            frame.instructions.push(Instruction {
                constant: None,
                instr: call,
            })
        }
        Ast::Statement(expr) => {
            compile_expression(*expr, frame)?;
            frame.instructions.push(Instruction {
                constant: None,
                instr: truncate,
            })
        }
        node => panic!("Unsupported AST node {:?}", node),
    }

    Ok(())
}

fn compile_function<'a>(
    name: &'a str,
    parent: Option<&'a Frame<'a>>,
    arguments: Vec<&'a str>,
    statements: Vec<Ast<'a>>,
) -> Result<Frame<'a>> {
    let mut frame: Frame<'a> = Frame {
        instructions: vec![],
        locals: arguments,
        parent,
        functions: vec![],
    };

    for statement in statements.into_iter() {
        compile_expression(statement, &mut frame)
            .with_context(|| format!("Error compiling function {}", name))?;
    }

    frame.instructions.push(Instruction {
        constant: Some(StaticValue::Undefined),
        instr: ret,
    });

    println!("Compilation output {:?}", frame.locals);

    Ok(frame)
}

pub(crate) fn compile(id: &str, input: ParsedModule) -> Result<Module> {
    let frame = compile_function(id, None, Vec::new(), input.statements)
        .with_context(|| format!("Error compiling module {}", id))?;

    Ok(Module {
        init: Function {
            instructions: frame.instructions,
            stack_size: 0,
            local_size: frame.locals.len(),
            functions: frame.functions,
            name: "<init>".to_owned(),
        },
    })
}
