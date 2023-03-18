use crate::compiler::{ChunkBuilder, Compile};
use crate::parser::ast::UnaryOperator;
use crate::result::Result;
use instruction_set::Instruction::*;

impl Compile for UnaryOperator {
    fn compile(self, builder: ChunkBuilder) -> Result<ChunkBuilder> {
        let instruction = match self {
            UnaryOperator::TypeOf => Some(TypeOf),
            UnaryOperator::LogicalNot => Some(LogicalNot),
            UnaryOperator::Sub => Some(Neg),
            UnaryOperator::Add => None,
            UnaryOperator::Delete => Some(Delete),
            UnaryOperator::PrefixInc => Some(Increment { by: 1.0, pre: true }),
            UnaryOperator::PrefixDec => Some(Increment {
                by: -1.0,
                pre: true,
            }),
        };

        if let Some(instruction) = instruction {
            Ok(builder.append(instruction))
        } else {
            Ok(builder)
        }
    }
}
