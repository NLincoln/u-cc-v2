use core::fmt::Pointer;
use lang::symbol::SymbolTable;
use lang::walker::{AstWalker, ExprWalker};
use lang::{
    AssignmentOp, BinaryOp, FunctionDeclaration, FunctionDefinition, Program, Statement, Type,
    VariableDefinition,
};
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    /// A temporary symbol, not part of the original program
    Temp(u32),
    /// A string literal: really an index
    /// into the list of literals on the CodeGen struct
    StringLiteral(usize),
    /// Exactly what it says on the tin
    NumericLiteral(i32),
    /// An identifier
    Ident(String),
    /// a[b]
    Index(Box<Symbol>, Box<Symbol>),
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Temp(num) => write!(f, "_t{}", num),
            Symbol::StringLiteral(s) => write!(f, "'{}'", s),
            Symbol::NumericLiteral(num) => write!(f, "{}", num),
            Symbol::Ident(ident) => ident.fmt(f),
            Symbol::Index(a, b) => write!(f, "{}[{}]", a, b),
        }
    }
}

#[derive(Debug)]
pub enum Instruction {
    /// t0 = &a
    AddressOf(Symbol, Symbol),
    /// t0 = *a
    Deref(Symbol, Symbol),
    /// Simple Assignment
    /// a = b
    Assign(Symbol, Symbol),
    /// a = b + c
    Add(Symbol, Symbol, Symbol),

    Mul(Symbol, Symbol, Symbol),

    Return,
    /// Push the value in a symbol onto the stack used for function calls
    ArgPush(Symbol),
    /// Pops a value from the argument stack, placing it in the identifier
    ArgPop(Symbol),
    /// Jumps to the specified label
    Jump(String),
    /// Jumps to the specified label if the symbol is zero
    JumpZero(Symbol, String),
    Label(String),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::AddressOf(a, b) => write!(f, "{} = &{}", a, b),
            Instruction::Deref(a, b) => write!(f, "{} = *{}", a, b),
            Instruction::Assign(a, b) => write!(f, "{} = {}", a, b),
            Instruction::Add(lhs, a, b) => write!(f, "{} = {} + {}", lhs, a, b),
            Instruction::Mul(lhs, a, b) => write!(f, "{} = {} * {}", lhs, a, b),
            Instruction::Return => write!(f, "return"),
            Instruction::ArgPush(sym) => write!(f, "apush {}", sym),
            Instruction::ArgPop(sym) => write!(f, "apop {}", sym),
            Instruction::Jump(sym) => write!(f, "goto {}", sym),
            Instruction::JumpZero(symbol, string) => write!(f, "jz {}, {}", symbol, string),
            Instruction::Label(label) => write!(f, "{}:", label),
        }
    }
}

pub struct CompiledFunction {
    declaration: FunctionDeclaration,
    instructions: Vec<Instruction>,
}

impl CompiledFunction {
    fn new(decl: FunctionDeclaration) -> CompiledFunction {
        CompiledFunction {
            declaration: decl,
            instructions: vec![],
        }
    }
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
    pub fn declaration(&self) -> &FunctionDeclaration {
        &self.declaration
    }
}

#[derive(Default)]
struct CodeGen {
    table: SymbolTable<Symbol>,
    string_literals: Vec<String>,
    temp_counter: u32,
    functions: Vec<CompiledFunction>,
    current_function: Option<CompiledFunction>,
}

impl CodeGen {
    fn gen<I: Into<Instruction>>(&mut self, ins: I) {
        self.current_function
            .as_mut()
            .unwrap()
            .instructions
            .push(ins.into());
    }
    fn gen_temp(&mut self) -> Symbol {
        let sym = Symbol::Temp(self.temp_counter);
        self.temp_counter += 1;
        sym
    }
    fn gen_label(&mut self) -> String {
        let temp = self.temp_counter;
        self.temp_counter += 1;
        format!("L{}", temp)
    }
}

impl ExprWalker for CodeGen {
    type Error = String;
    type Node = Symbol;

    fn visit_number(&mut self, num: i32) -> Result<Self::Node, Self::Error> {
        Ok(Symbol::NumericLiteral(num))
    }

    fn visit_string_literal(&mut self, literal: &str) -> Result<Self::Node, Self::Error> {
        let idx = self.string_literals.len();
        self.string_literals.push(literal.to_string());
        Ok(Symbol::StringLiteral(idx))
    }

    fn visit_ident(&mut self, ident: &str) -> Result<Self::Node, Self::Error> {
        Ok(Symbol::Ident(ident.to_string()))
    }

    fn visit_address_of(&mut self, ident: &str) -> Result<Self::Node, Self::Error> {
        let temp = self.gen_temp();
        self.gen(Instruction::AddressOf(
            temp.clone(),
            Symbol::Ident(ident.to_string()),
        ));
        Ok(temp)
    }

    fn visit_index(
        &mut self,
        expr: Self::Node,
        index: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        let temp = self.gen_temp();
        self.gen(Instruction::Assign(
            temp.clone(),
            Symbol::Index(expr.into(), index.into()),
        ));
        Ok(temp)
    }

    fn visit_arrow_property(
        &mut self,
        lhs: Self::Node,
        ident: &str,
    ) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_cast(&mut self, ty: &Type, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_dereference(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        let temp = self.gen_temp();
        self.gen(Instruction::Deref(temp.clone(), expr));
        Ok(temp)
    }

    fn visit_assignment(
        &mut self,
        lhs: Self::Node,
        op: &AssignmentOp,
        rhs: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        let res = match op {
            AssignmentOp::Assign => rhs.clone(),
            AssignmentOp::AddAssign => {
                let temp = self.gen_temp();
                self.gen(Instruction::Add(temp.clone(), lhs.clone(), rhs));
                temp
            }
            _ => unimplemented!(),
        };

        self.gen(Instruction::Assign(lhs.clone(), res));
        Ok(lhs)
    }

    fn visit_ternary(
        &mut self,
        cond: Self::Node,
        truthy: Self::Node,
        falsy: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_function_call(
        &mut self,
        name: &str,
        args: Vec<Self::Node>,
    ) -> Result<Self::Node, Self::Error> {
        for arg in args {
            self.gen(Instruction::ArgPush(arg));
        }
        self.gen(Instruction::Jump(name.to_string()));
        let temp = self.gen_temp();
        self.gen(Instruction::ArgPop(temp.clone()));
        Ok(temp)
    }

    fn visit_dot_property(
        &mut self,
        expr: Self::Node,
        ident: &str,
    ) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_sizeof_expr(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_sizeof_type(&mut self, ty: &Type) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_not(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_plus(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_negation(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_binary_op(
        &mut self,
        lhs: Self::Node,
        op: &BinaryOp,
        rhs: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        let temp = self.gen_temp();
        match op {
            BinaryOp::Add => {
                self.gen(Instruction::Add(temp.clone(), lhs, rhs));
            }
            BinaryOp::Mul => {
                self.gen(Instruction::Mul(temp.clone(), lhs, rhs));
            }
            _ => unimplemented!(),
        }
        Ok(temp)
    }

    fn visit_post_increment(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        let temp = self.gen_temp();
        self.gen(Instruction::Assign(temp.clone(), expr.clone()));
        self.gen(Instruction::Add(
            expr.clone(),
            expr.clone(),
            Symbol::NumericLiteral(1),
        ));
        Ok(temp)
    }

    fn visit_post_decrement(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        let temp = self.gen_temp();
        self.gen(Instruction::Assign(temp.clone(), expr.clone()));
        self.gen(Instruction::Add(
            expr.clone(),
            expr.clone(),
            Symbol::NumericLiteral(-1),
        ));
        Ok(temp)
    }

    fn visit_prefix_increment(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_prefix_decrement(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }
}

impl AstWalker for CodeGen {
    type AstError = String;

    fn enter_scope(&mut self) {
        self.table.push_scope()
    }

    fn exit_scope(&mut self) {
        self.table.pop_scope()
    }

    fn visit_function_definition(
        &mut self,
        func: &FunctionDefinition,
    ) -> Result<(), Self::AstError> {
        self.current_function = Some(CompiledFunction::new(func.declaration.clone()));
        for statement in func.body.iter() {
            self.walk_statement(statement)?;
        }
        self.functions.push(self.current_function.take().unwrap());
        Ok(())
    }
    fn visit_return_statement(&mut self, expr: Self::Node) -> Result<(), Self::AstError> {
        self.gen(Instruction::ArgPush(expr));
        self.gen(Instruction::Return);
        Ok(())
    }

    fn visit_variable_definition(
        &mut self,
        def: &VariableDefinition,
    ) -> Result<(), Self::AstError> {
        let val = self.visit_expr(&def.value)?;
        self.gen(Instruction::Assign(
            Symbol::Ident(def.declaration.name.to_string()),
            val,
        ));
        Ok(())
    }
    fn visit_if(
        &mut self,
        cond: Self::Node,
        yes: &[Statement],
        no: Option<&[Statement]>,
    ) -> Result<(), Self::AstError> {
        let no_label = self.gen_label();
        self.gen(Instruction::JumpZero(cond, no_label.clone()));
        for stmt in yes {
            self.walk_statement(stmt)?;
        }

        self.gen(Instruction::Label(no_label));

        if let Some(no) = no {
            for stmt in no {
                self.walk_statement(stmt)?;
            }
        }
        Ok(())
    }
}

pub fn codegen(ast: &Program) -> Result<Vec<CompiledFunction>, String> {
    let mut gen = CodeGen::default();
    lang::walker::walk(ast, &mut gen)?;
    Ok(gen.functions)
}
