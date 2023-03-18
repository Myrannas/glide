use crate::compiler::{ChunkBuilder, Compile};
use crate::parser::ast::BinaryOperator;
use instruction_set::Instruction::*;

impl Compile for BinaryOperator {
    fn compile(self, builder: ChunkBuilder) -> crate::result::Result<ChunkBuilder> {
        let instruction = match self {
            BinaryOperator::Add => Add { times: 1 },
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
            BinaryOperator::In => In,
            BinaryOperator::Exponential => Exponential,
            BinaryOperator::LogicalOr => panic!("Lor is handled separately"),
            BinaryOperator::LogicalAnd => panic!("Land is handled separately"),
        };

        Ok(builder.append(instruction))
    }
}
