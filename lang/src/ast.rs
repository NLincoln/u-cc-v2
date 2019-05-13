#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionStorageClass {
    Extern,
    Static,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub ty: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub return_type: Type,
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
    pub storage_class: Option<FunctionStorageClass>,
    pub body: Option<Vec<Statement>>,
}

impl FunctionDefinition {
    pub fn type_of(&self) -> Type {
        Type::Function {
            return_type: Box::new(self.return_type.clone()),
            arguments: self.parameters.iter().map(|arg| arg.ty.clone()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Function {
        return_type: Box<Type>,
        arguments: Vec<Type>,
    },
    Pointer(Box<Type>),
}

impl Type {
    pub fn stack_size(&self) -> usize {
        match self {
            // ok I mean this is probably the worst way to do this but whatever.
            Type::Int => 4,
            Type::Function { .. } => 0,
            Type::Pointer(_) => 8,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Box<Expr>),
    Expr(Box<Expr>),
    VariableDefinition {
        ty: Type,
        name: String,
        value: Box<Expr>,
    },
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
