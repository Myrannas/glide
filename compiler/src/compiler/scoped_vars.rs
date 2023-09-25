use crate::compiler::ast_visitor::AstVisitor;
use crate::compiler::locals::LocalAllocator;
use crate::parser::ast::{FunctionStatement, VarDeclaration};

pub(crate) struct VarVisitor<'a, 'b> {
    pub(crate) vars: &'a mut LocalAllocator<'b>,
}

impl<'a, 'b> AstVisitor<'a> for VarVisitor<'a, 'b> {
    fn visit_function_declaration(&mut self, _function: &FunctionStatement<'a>) -> bool {
        false
    }

    fn visit_var_declaration(&mut self, decl: &VarDeclaration<'a>) -> bool {
        self.vars.allocate_identifier(decl.identifier, false);
        true
    }
}
