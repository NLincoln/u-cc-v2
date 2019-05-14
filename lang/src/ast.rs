#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub ty: Type,
    pub name: String,
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
}

impl Type {
    fn is_numeric(&self) -> bool {
        match self {
            Type::Signed(_) | Type::Unsigned(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDefinition {
    pub ty: Type,
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Box<Expr>),
    Expr(Box<Expr>),
    VariableDefinition(VariableDefinition),
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
