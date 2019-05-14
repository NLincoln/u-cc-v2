use lang::ast::*;
use lang::walker::{AstWalker, ExprWalker};
use std::collections::HashMap;

struct TypeChecker {
    scopes: Vec<HashMap<String, Type>>,
}

fn is_numeric_type(ty: &Type) -> bool {
    match ty {
        Type::Pointer(_) => true,
        Type::Unsigned(_) | Type::Signed(_) => true,
        other => false,
    }
}

fn guard_numeric_type(ty: &Type) -> Result<(), String> {
    if is_numeric_type(ty) {
        return Ok(());
    }
    return Err(format!("Expected a numeric type, received {:?}", ty));
}

fn is_unsigned_int(ty: &Type) -> bool {
    match ty {
        Type::Unsigned(NumericType::Int) => true,
        _ => false,
    }
}

fn types_are_compatible(a: &Type, b: &Type) -> bool {
    if is_numeric_type(a) && is_numeric_type(b) {
        return true;
    }
    return false;
}

fn integer_promotion(a: &Type, b: &Type) -> Type {
    if is_unsigned_int(a) || is_unsigned_int(b) {
        return Type::Unsigned(NumericType::Int);
    }
    return Type::Signed(NumericType::Int);
}

impl TypeChecker {
    fn new() -> TypeChecker {
        TypeChecker {
            scopes: vec![Default::default()],
        }
    }
    fn current_scope(&mut self) -> &mut HashMap<String, Type> {
        self.scopes.last_mut().unwrap()
    }
    fn lookup(&self, ident: &str) -> Option<&Type> {
        for scope in self.scopes.iter() {
            match scope.get(ident) {
                found @ Some(_) => return found,
                None => {}
            }
        }
        None
    }
}
impl ExprWalker for TypeChecker {
    type Error = String;
    type Node = Type;

    fn visit_number(&mut self, num: i32) -> Result<Self::Node, Self::Error> {
        Ok(Type::Signed(NumericType::Int))
    }

    fn visit_string_literal(&mut self, literal: &str) -> Result<Self::Node, Self::Error> {
        Ok(Type::Pointer(Type::Signed(NumericType::Char).into()))
    }

    fn visit_ident(&mut self, ident: &str) -> Result<Self::Node, Self::Error> {
        self.lookup(ident)
            .cloned()
            .ok_or_else(|| format!("Could not find identifier {}", ident))
    }

    fn visit_address_of(&mut self, ident: &str) -> Result<Self::Node, Self::Error> {
        Ok(Type::Pointer(
            self.lookup(ident)
                .cloned()
                .ok_or_else(|| format!("Could not find identifier {}", ident))?
                .into(),
        ))
    }

    fn visit_arrow_property(
        &mut self,
        lhs: Self::Node,
        ident: &str,
    ) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_cast(&mut self, ty: &Type, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        Ok(ty.clone())
    }

    fn visit_dereference(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        match expr {
            Type::Pointer(inner) => Ok(*inner),
            _ => Err(format!("Cannot dereference non-pointer argument")),
        }
    }

    fn visit_assignment(
        &mut self,
        lhs: Self::Node,
        op: &AssignmentOp,
        rhs: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        if lhs == rhs {
            Ok(lhs)
        } else {
            Err(format!(
                "Cannot assign a {:?} to a value of type {:?}",
                rhs, lhs
            ))
        }
    }

    fn visit_ternary(
        &mut self,
        cond: Self::Node,
        truthy: Self::Node,
        falsy: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        if truthy == falsy {
            Ok(truthy)
        } else {
            Err(format!(
                "Both sides of a ternary must match: got {:?}, and {:?}",
                truthy, falsy
            ))
        }
    }

    fn visit_function_call(
        &mut self,
        name: &str,
        passed_args: Vec<Self::Node>,
    ) -> Result<Self::Node, Self::Error> {
        let ty = self
            .lookup(name)
            .ok_or_else(|| format!("Could not find function {}", name))?;
        let (return_type, needed_args) = match ty {
            Type::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => return Err(format!("Cannot call {} as a function", name)),
        };

        if passed_args.len() != needed_args.len() {
            return Err(format!(
                "Incorrect number of arguments to function {}: Needed {}, got {}",
                name,
                needed_args.len(),
                passed_args.len()
            ));
        }

        for (passed, needed) in passed_args.iter().zip(needed_args.iter()) {
            if !types_are_compatible(passed, needed) {
                return Err(format!(
                    "Tried to pass an incorrect type to {}: Needed {:?}, got {:?}",
                    name, needed, passed
                ));
            }
        }

        Ok(*return_type.clone())
    }

    fn visit_dot_property(
        &mut self,
        expr: Self::Node,
        ident: &str,
    ) -> Result<Self::Node, Self::Error> {
        unimplemented!()
    }

    fn visit_sizeof_expr(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        Ok(Type::Signed(NumericType::Int))
    }

    fn visit_sizeof_type(&mut self, ty: &Type) -> Result<Self::Node, Self::Error> {
        Ok(Type::Signed(NumericType::Int))
    }

    fn visit_not(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }

    fn visit_plus(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }

    fn visit_negation(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }

    fn visit_binary_op(
        &mut self,
        lhs: Self::Node,
        op: &BinaryOp,
        rhs: Self::Node,
    ) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&lhs)?;
        guard_numeric_type(&rhs)?;
        Ok(integer_promotion(&lhs, &rhs))
    }

    fn visit_post_increment(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }

    fn visit_post_decrement(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }

    fn visit_prefix_increment(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }

    fn visit_prefix_decrement(&mut self, expr: Self::Node) -> Result<Self::Node, Self::Error> {
        guard_numeric_type(&expr)?;
        Ok(expr)
    }
}

impl AstWalker for TypeChecker {
    fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
    fn visit_function_declaration(
        &mut self,
        func: &FunctionDeclaration,
    ) -> Result<(), Self::Error> {
        self.current_scope()
            .insert(func.name.clone(), func.type_of());
        Ok(())
    }
    fn visit_variable_definition(&mut self, def: &VariableDefinition) -> Result<(), Self::Error> {
        let type_of = self.walk_expr(&def.value)?;

        self.current_scope()
            .insert(def.name.to_string(), def.ty.clone());
        Ok(())
    }
}

pub fn type_check(ast: &Program) -> Result<(), String> {
    lang::walker::walk(ast, &mut TypeChecker::new())
}
