use crate::ast::*;

pub trait ExprWalker {
    type Error;
    type Node;

    fn walk_expr(&mut self, expr: &Expr) -> Result<Self::Node, Self::Error> {
        match expr {
            Expr::Number(num) => self.visit_number(*num),
            Expr::RawString(literal) => self.visit_string_literal(literal),
            Expr::Ident(ident) => self.visit_ident(ident),
            Expr::AddressOf(ident) => self.visit_address_of(ident),
            Expr::Index(expr, index) => {
                let expr = self.walk_expr(expr)?;
                let index = self.walk_expr(index)?;
                self.visit_index(expr, index)
            }
            Expr::ArrowProperty(lhs, ident) => {
                let lhs = self.walk_expr(lhs)?;
                self.visit_arrow_property(lhs, ident)
            }

            Expr::Cast(ty, expr) => {
                let expr = self.walk_expr(expr)?;
                self.visit_cast(ty, expr)
            }
            Expr::Ternary {
                cond,
                truthy,
                falsey,
            } => {
                let cond = self.walk_expr(cond)?;
                let truthy = self.walk_expr(truthy)?;
                let falsey = self.walk_expr(falsey)?;
                self.visit_ternary(cond, truthy, falsey)
            }
            Expr::Assignment { lhs, op, value } => {
                let value = self.walk_expr(value)?;
                let lhs = self.walk_expr(lhs)?;
                self.visit_assignment(lhs, op, value)
            }
            Expr::Dereference(expr) => {
                let expr = self.walk_expr(expr)?;
                self.visit_dereference(expr)
            }
            Expr::FunctionCall(call) => {
                let args = call
                    .arguments
                    .iter()
                    .map(|arg| self.walk_expr(arg))
                    .collect::<Result<Vec<_>, Self::Error>>()?;
                self.visit_function_call(&call.name, args)
            }
            Expr::Op(lhs, op, rhs) => {
                let lhs = self.walk_expr(lhs)?;
                let rhs = self.walk_expr(rhs)?;
                self.visit_binary_op(lhs, op, rhs)
            }
            Expr::PostIncrement(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_post_increment(node)
            }
            Expr::PostDecrement(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_post_decrement(node)
            }
            Expr::DotProperty(expr, ident) => {
                let node = self.walk_expr(expr)?;
                self.visit_dot_property(node, ident)
            }
            Expr::SizeofExpr(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_sizeof_expr(node)
            }
            Expr::SizeofType(ty) => self.visit_sizeof_type(ty),
            Expr::Not(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_not(node)
            }
            Expr::Plus(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_plus(node)
            }
            Expr::Neg(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_negation(node)
            }
            Expr::PrefixIncrement(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_prefix_increment(node)
            }
            Expr::PrefixDecrement(inner) => {
                let node = self.walk_expr(inner)?;
                self.visit_prefix_decrement(node)
            }
        }
    }

    fn visit_number(&mut self, num: i32) -> Result<Self::Node, Self::Error>;
    fn visit_string_literal(&mut self, literal: &str) -> Result<Self::Node, Self::Error>;
    fn visit_ident(&mut self, ident: &str) -> Result<Self::Node, Self::Error>;

    fn visit_address_of(&mut self, ident: &str) -> Result<Self::Node, Self::Error>;
    fn visit_index(
        &mut self,
        expr: Self::Node,
        index: Self::Node,
    ) -> Result<Self::Node, Self::Error>;

    fn visit_arrow_property(
        &mut self,
        lhs: Self::Node,
        ident: &str,
    ) -> Result<Self::Node, Self::Error>;
    fn visit_cast(&mut self, ty: &Type, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_dereference(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_assignment(
        &mut self,
        lhs: Self::Node,
        op: &AssignmentOp,
        rhs: Self::Node,
    ) -> Result<Self::Node, Self::Error>;

    fn visit_ternary(
        &mut self,
        cond: Self::Node,
        truthy: Self::Node,
        falsy: Self::Node,
    ) -> Result<Self::Node, Self::Error>;

    fn visit_function_call(
        &mut self,
        name: &str,
        args: Vec<Self::Node>,
    ) -> Result<Self::Node, Self::Error>;

    fn visit_dot_property(
        &mut self,
        expr: Self::Node,
        ident: &str,
    ) -> Result<Self::Node, Self::Error>;

    fn visit_sizeof_expr(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_sizeof_type(&mut self, ty: &Type) -> Result<Self::Node, Self::Error>;

    fn visit_not(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_plus(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_negation(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;

    fn visit_binary_op(
        &mut self,
        lhs: Self::Node,
        op: &BinaryOp,
        rhs: Self::Node,
    ) -> Result<Self::Node, Self::Error>;

    fn visit_post_increment(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_post_decrement(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;

    fn visit_prefix_increment(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
    fn visit_prefix_decrement(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error>;
}

/// A method for executing some code at different points in an AST
///
/// Walking the AST of a C program can be difficult. There are several different
/// things that must be handled, and the control flow can get a bit gnarly. This
/// uses something known as the "visitor" pattern to only handle the parts
/// of the AST you care about.
///
/// At each step, you can return some error to stop the evaluation of the AST. This
/// is useful for instances like typechecking, where you'll want to stop after the first error
/// (note that a more robust implementation would allow the walker to decide exactly how "far out"
/// to bail).
pub trait AstWalker: ExprWalker {
    type AstError: From<<Self as ExprWalker>::Error>;

    fn enter_scope(&mut self);
    fn exit_scope(&mut self);
    /// "Visits" the entire program
    ///
    /// This will be called once at the beginning of the ast walk. Usually you don't need this,
    /// however
    fn visit_program(&mut self, program: &Program) -> Result<(), Self::AstError> {
        Ok(())
    }

    /// Visits a function _declaration_
    ///
    /// This will get called for every function declaration in the program, including
    /// those with a body (aka function definitions). As such, this may be called multiple times
    /// for a function with the same name throughout the program.
    fn visit_function_declaration(
        &mut self,
        func: &FunctionDeclaration,
    ) -> Result<(), Self::AstError> {
        Ok(())
    }

    /// Called when we're starting to visit the actual body of the function. If you need to do
    /// some analysis of the entirety of a function, it's best to do it here.
    fn visit_function_definition(
        &mut self,
        func: &FunctionDefinition,
    ) -> Result<(), Self::AstError> {
        for statement in func.body.iter() {
            self.walk_statement(statement)?;
        }
        Ok(())
    }

    /// Called for each statement in the body of a function
    fn visit_statement(&mut self, stmt: &Statement) -> Result<(), Self::AstError> {
        Ok(())
    }

    fn visit_return_statement(&mut self, expr: Self::Node) -> Result<(), Self::AstError> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::Node, Self::AstError> {
        Ok(self.walk_expr(expr)?)
    }

    fn visit_variable_definition(
        &mut self,
        def: &VariableDefinition,
    ) -> Result<(), Self::AstError> {
        self.visit_expr(&def.value)?;
        Ok(())
    }
    fn visit_variable_declaration(
        &mut self,
        decl: &VariableDeclaration,
    ) -> Result<(), Self::AstError> {
        Ok(())
    }

    fn visit_if(
        &mut self,
        cond: Self::Node,
        yes: &[Statement],
        no: Option<&[Statement]>,
    ) -> Result<(), Self::AstError> {
        for stmt in yes {
            self.walk_statement(stmt)?;
        }
        if let Some(no) = no {
            for stmt in no {
                self.walk_statement(stmt)?;
            }
        }
        Ok(())
    }

    fn visit_break(&mut self) -> Result<(), Self::AstError> {
        Ok(())
    }
    fn visit_continue(&mut self) -> Result<(), Self::AstError> {
        Ok(())
    }

    fn visit_while(&mut self, while_stmt: &While) -> Result<(), Self::AstError> {
        self.visit_expr(&while_stmt.cond)?;
        for stmt in while_stmt.body.iter() {
            self.walk_statement(stmt)?;
        }
        Ok(())
    }

    fn visit_do_while(&mut self, do_while: &DoWhile) -> Result<(), Self::AstError> {
        for stmt in do_while.body.iter() {
            self.walk_statement(stmt)?;
        }
        self.visit_expr(&do_while.cond)?;
        Ok(())
    }

    fn walk_statement(&mut self, statement: &Statement) -> Result<(), Self::AstError> {
        self.visit_statement(statement)?;

        match statement {
            Statement::VariableDefinition(def) => {
                self.visit_variable_declaration(&def.declaration)?;
                self.visit_variable_definition(def)?;
            }
            Statement::VariableDeclaration(decl) => {
                self.visit_variable_declaration(decl)?;
            }
            Statement::Expr(inner) => {
                self.walk_expr(inner)?;
            }
            Statement::Return(inner) => {
                let inner = self.walk_expr(inner)?;
                self.visit_return_statement(inner)?;
            }
            Statement::If(If { cond, yes, no }) => {
                let cond = self.walk_expr(cond)?;
                self.visit_if(cond, yes, no.as_ref().map(AsRef::as_ref))?;
            }
            Statement::Break => {
                self.visit_break()?;
            }
            Statement::Continue => {
                self.visit_continue()?;
            }
            Statement::While(while_stmt) => {
                self.visit_while(while_stmt)?;
            }
            Statement::DoWhile(do_while) => {
                self.visit_do_while(do_while)?;
            }
        };

        Ok(())
    }
}

pub fn walk<W: AstWalker>(ast: &Program, walker: &mut W) -> Result<(), W::AstError> {
    walker.visit_program(ast)?;

    for function in ast.functions.iter() {
        walker.visit_function_declaration(function.declaration())?;
        if let Function::Definition(def) = function {
            walker.visit_function_definition(def)?;
        }
    }
    Ok(())
}
