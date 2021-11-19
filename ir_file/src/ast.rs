#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AST {
    pub sections: Vec<Section>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Section {
    pub header: Header,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Header {
    pub document_index: String,
    pub method_name: FnName,
    pub parameters: Vec<Variable>,
}

type Variable = String;
type FnName = String;
type Slot = String;
type TrivialItem = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign {
    pub variable: Variable,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Assign(Assign),
    /// An if statement as an expression is different than an if statement as a
    /// statement becuase an if statement as an expression MUST have a carry
    /// value, whereas an if statement as a statement does not.
    If {
        condition: Expression,
        then: Vec<Statement>,
        r#else: Option<Vec<Statement>>,
    },
    RecordSetProp {
        record: Expression,
        prop: Expression,
        value: Option<Expression>,
    },
    RecordSetSlot {
        record: Expression,
        slot: Slot,
        value: Option<Expression>,
    },
    ListSet {
        list: Expression,
        prop: Expression,
        value: Option<Expression>,
    },
    Return {
        expr: Option<Expression>,
    },
    // TODO(irfile): should we have a comment instruction?
    // Comment {
    //     message: String,
    //     location: Span,
    // },
    /// A call as an expression is different from a call as a statement, because
    /// an expression call expects a value whereas statement call does not.
    CallStatic {
        function_name: FnName,
        args: Vec<Expression>,
    },
    // TODO(isa): implement calling external functions
    // CallExternal {},
    CallVirt {
        fn_ptr: Expression,
        args: Vec<Expression>,
    },
    Assert {
        expr: Expression,
        message: String,
    },
    Loop {
        init: Vec<Assign>,
        cond: Expression,
        next: Vec<Assign>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    /// If enabled, there is a piece of "threaded state" which will
    /// automatically thread itself through every single function declaration
    /// and call. This is an instruction to get that piece of threaded state.
    GetGlobal,
    /// An if statement as an expression is different than an if statement as a
    /// statement becuase an if statement as an expression MUST have a carry
    /// value, whereas an if statement as a statement does not.
    If {
        condition: Box<Expression>,
        then: (Vec<Statement>, Box<Expression>),
        r#else: (Vec<Statement>, Box<Expression>),
    },
    VarReference {
        variable: Variable,
    },
    LetIn {
        variable: Variable,
        be_bound_to: Box<Expression>,
        r#in: (Vec<Statement>, Box<Expression>),
    },
    Unreachable,
    RecordNew,
    RecordGetProp {
        record: Box<Expression>,
        property: Box<Expression>,
    },
    RecordGetSlot {
        record: Box<Expression>,
        slot: Slot,
    },
    RecordHasProp {
        record: Box<Expression>,
        property: Box<Expression>,
    },
    RecordHasSlot {
        record: Box<Expression>,
        slot: Slot,
    },
    ListNew,
    ListGet {
        list: Box<Expression>,
        property: Box<Expression>,
    },
    ListHas {
        list: Box<Expression>,
        property: Box<Expression>,
    },
    ListLen {
        list: Box<Expression>,
    },
    GetFnPtr {
        function_name: FnName,
    },
    /// A call as an expression is different from a call as a statement, because
    /// an expression call expects a value whereas statement call does not.
    CallStatic {
        function_name: FnName,
        args: Vec<Expression>,
    },
    // TODO(isa): implement calling external functions
    // CallExternal {},
    CallVirt {
        fn_ptr: Box<Expression>,
        args: Vec<Expression>,
    },
    MakeTrivial {
        trivial_item: TrivialItem,
    },
    MakeBytes {
        bytes: Vec<u8>,
    },
    MakeInteger {
        value: i64,
    },
    MakeBoolean {
        value: bool,
    },
    BinOp {
        kind: BinOpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Negate {
        expr: Box<Expression>,
    },
    IsTypeOf {
        expr: Box<Expression>,
        kind: String,
    },
    IsTypeAs {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    And,
    Or,
    Eq,
    Lt,
}
