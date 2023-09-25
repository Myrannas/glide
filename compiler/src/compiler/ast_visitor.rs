use crate::parser::ast::{
    BlockStatement, FunctionStatement, IfStatement, VarDeclaration, VarStatement,
};
use crate::parser::statements::decl_statement::DeclStatement;
use crate::parser::statements::for_statement::ForStatement;
use crate::parser::statements::statement::Statement;

pub(crate) trait AstVisitor<'a> {
    fn visit_function_declaration(&mut self, _function: &FunctionStatement<'a>) -> bool {
        true
    }

    fn visit_if_statement(&mut self, _if_statement: &IfStatement<'a>) -> bool {
        true
    }

    fn visit_block(&mut self, _block: &BlockStatement<'a>) -> bool {
        true
    }

    fn visit_var_statement(&mut self, _var: &VarStatement<'a>) -> bool {
        true
    }
    fn visit_for_statement(&mut self, _var: &ForStatement<'a>) -> bool {
        true
    }

    fn visit_var_declaration(&mut self, _decl: &VarDeclaration<'a>) -> bool {
        true
    }
}

pub(crate) trait Visit<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T);
}

impl<'a> Visit<'a> for DeclStatement<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        match self {
            DeclStatement::Class(_) => {}
            DeclStatement::Function(function) => {
                function.visit(visitor);
            }
            DeclStatement::Statement(statement) => {
                statement.visit(visitor);
            }
            DeclStatement::Const(_) => {}
        }
    }
}

impl<'a> Visit<'a> for FunctionStatement<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        if !visitor.visit_function_declaration(self) {
            return;
        }

        self.statements.visit(visitor);
    }
}

impl<'a> Visit<'a> for BlockStatement<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        if !visitor.visit_block(self) {
            return;
        }

        self.statements.visit(visitor)
    }
}

impl<'a> Visit<'a> for Statement<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        match self {
            Statement::Label(_, _) => {}
            Statement::Block(_) => {}
            Statement::If(_) => {}
            Statement::Return(_) => {}
            Statement::While(_) => {}
            Statement::DoWhile(_) => {}
            Statement::Var(statement) => {
                statement.visit(visitor);
            }
            Statement::Expression(_) => {}
            Statement::Try(_) => {}
            Statement::For(_) => {}
            Statement::Break(_) => {}
            Statement::Continue(_) => {}
            Statement::ThrowStatement(_) => {}
            Statement::Empty => {}
        }
    }
}

impl<'a> Visit<'a> for VarStatement<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        if !visitor.visit_var_statement(self) {
            return;
        }

        self.declarations.visit(visitor)
    }
}

impl<'a> Visit<'a> for VarDeclaration<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        visitor.visit_var_declaration(self);
    }
}

impl<'a, V: Visit<'a>> Visit<'a> for Vec<V> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        for item in self {
            item.visit(visitor)
        }
    }
}

impl<'a, V: Visit<'a>> Visit<'a> for Option<V> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        if let Some(node) = self {
            node.visit(visitor)
        }
    }
}

impl<'a> Visit<'a> for ForStatement<'a> {
    fn visit<T: AstVisitor<'a>>(&self, visitor: &mut T) {
        visitor.visit_for_statement(self);

        match self {
            ForStatement::For { vars, .. } => vars.visit(visitor),
            ForStatement::ForIn { .. } => {}
        }
    }
}
