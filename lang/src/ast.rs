use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub ty: Type,
    pub name: String,
}

impl Display for FunctionParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Definition(FunctionDefinition),
    Declaration(FunctionDeclaration),
}

impl Function {
    pub fn declaration(&self) -> &FunctionDeclaration {
        match self {
            Function::Declaration(decl) => decl,
            Function::Definition(def) => &def.declaration,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub return_type: Type,
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
}

impl FunctionDeclaration {
    pub fn type_of(&self) -> Type {
        Type::Function {
            return_type: Box::new(self.return_type.clone()),
            arguments: self.parameters.iter().map(|arg| arg.ty.clone()).collect(),
        }
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.return_type, self.name)?;
        write!(f, "(")?;
        for (i, param) in self.parameters.iter().enumerate() {
            write!(f, "{}", param)?;
            if i != self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub declaration: FunctionDeclaration,
    pub body: Vec<Statement>,
}

impl FunctionDefinition {
    pub fn return_type(&self) -> &Type {
        &self.declaration.return_type
    }
    pub fn parameters(&self) -> &[FunctionParameter] {
        &self.declaration.parameters
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumericType {
    Char,
    Short,
    Int,
}

impl Display for NumericType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NumericType::*;
        let name = match self {
            Char => "char",
            Short => "short",
            Int => "int",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,

    Signed(NumericType),
    Unsigned(NumericType),

    Function {
        return_type: Box<Type>,
        arguments: Vec<Type>,
    },
    Pointer(Box<Type>),
    Array(i32, Box<Type>),
}

impl Type {
    fn is_numeric(&self) -> bool {
        match self {
            Type::Signed(_) | Type::Unsigned(_) => true,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;
        match self {
            Void => write!(f, "void"),
            Signed(num) | Unsigned(num) => write!(f, "{}", num),
            Function {
                return_type,
                arguments,
            } => {
                write!(f, "{}", return_type)?;
                write!(f, "(");
                for (i, arg) in arguments.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != arguments.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Pointer(inner) => write!(f, "{}*", inner),
            Array(len, inner) => write!(f, "{}[{}]", inner, len),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub ty: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDefinition {
    pub declaration: VariableDeclaration,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub yes: Vec<Statement>,
    pub no: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub cond: Box<Expr>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhile {
    pub cond: Box<Expr>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Box<Expr>),
    Expr(Box<Expr>),
    VariableDefinition(VariableDefinition),
    VariableDeclaration(VariableDeclaration),
    Break,
    Continue,
    If(If),
    While(While),
    DoWhile(DoWhile),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// 1
    Number(i32),
    /// a
    Ident(String),
    /// "a"
    RawString(String),
    /// &a
    AddressOf(String),
    /// a[b]
    Index(Box<Expr>, Box<Expr>),
    /// *a
    Dereference(Box<Expr>),
    /// a()
    FunctionCall(FunctionCall),
    /// a + b
    Op(Box<Expr>, BinaryOp, Box<Expr>),
    /// a++
    PostIncrement(Box<Expr>),
    /// a--
    PostDecrement(Box<Expr>),
    /// a->b
    ArrowProperty(Box<Expr>, String),
    /// a.b
    DotProperty(Box<Expr>, String),
    /// sizeof a+b
    SizeofExpr(Box<Expr>),
    /// sizeof(int)
    SizeofType(Type),

    /// !a
    Not(Box<Expr>),
    /// +a
    Plus(Box<Expr>),

    /// -a
    Neg(Box<Expr>),
    /// ++a
    PrefixIncrement(Box<Expr>),
    /// --a
    PrefixDecrement(Box<Expr>),
    /// (long)a
    Cast(Type, Box<Expr>),
    /// true ? a : b
    Ternary {
        cond: Box<Expr>,
        truthy: Box<Expr>,
        falsey: Box<Expr>,
    },
    /// a = b
    Assignment {
        lhs: Box<Expr>,
        op: AssignmentOp,
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOp {
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    XorAssign,
    OrAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    LeftShift,
    RightShift,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Box<Expr>>,
}
