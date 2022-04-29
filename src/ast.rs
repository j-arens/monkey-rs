use std::fmt;

use crate::operator::Operator;

#[derive(Clone, Debug)]
pub enum Node {
  Expression(Expression),
  #[allow(dead_code)]
  Literal(Literal),
  Statement(Statement),
}

impl From<Expression> for Node {
  fn from(expr: Expression) -> Self {
    Node::Expression(expr)
  }
}

impl From<Literal> for Node {
  fn from(lit: Literal) -> Self {
    Node::Literal(lit)
  }
}

impl From<Statement> for Node {
  fn from(stmt: Statement) -> Self {
    Node::Statement(stmt)
  }
}

// -- Expressions -------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Expression {
  Boolean(BooleanExpression),
  Call(CallExpression),
  Ident(IdentExpression),
  If(IfExpression),
  Index(IndexExpression),
  Infix(InfixExpression),
  Literal(Literal),
  Prefix(PrefixExpression),
}

impl From<BooleanExpression> for Expression {
  fn from(bool_expr: BooleanExpression) -> Expression {
    Expression::Boolean(bool_expr)
  }
}

impl From<CallExpression> for Expression {
  fn from(call_expr: CallExpression) -> Expression {
    Expression::Call(call_expr)
  }
}

impl From<IdentExpression> for Expression {
  fn from(ident_expr: IdentExpression) -> Expression {
    Expression::Ident(ident_expr)
  }
}

impl From<IfExpression> for Expression {
  fn from(if_expr: IfExpression) -> Expression {
    Expression::If(if_expr)
  }
}

impl From<IndexExpression> for Expression {
  fn from(index_expr: IndexExpression) -> Expression {
    Expression::Index(index_expr)
  }
}

impl From<InfixExpression> for Expression {
  fn from(infx_expr: InfixExpression) -> Expression {
    Expression::Infix(infx_expr)
  }
}

impl From<PrefixExpression> for Expression {
  fn from(prefix_expr: PrefixExpression) -> Expression {
    Expression::Prefix(prefix_expr)
  }
}

impl From<Literal> for Expression {
  fn from(lit: Literal) -> Self {
    Expression::Literal(lit)
  }
}

impl From<ArrayLiteral> for Expression {
  fn from(array_lit: ArrayLiteral) -> Self {
    Expression::from(Literal::from(array_lit))
  }
}

impl From<FunctionLiteral> for Expression {
  fn from(fn_lit: FunctionLiteral) -> Self {
    Expression::from(Literal::from(fn_lit))
  }
}

impl From<HashLiteral> for Expression {
  fn from(hash_lit: HashLiteral) -> Self {
    Expression::from(Literal::from(hash_lit))
  }
}

impl From<NumberLiteral> for Expression {
  fn from(num_lit: NumberLiteral) -> Self {
    Expression::from(Literal::from(num_lit))
  }
}

impl From<StringLiteral> for Expression {
  fn from(str_lit: StringLiteral) -> Self {
    Expression::from(Literal::from(str_lit))
  }
}

// Boolean Expression

#[derive(Clone)]
pub struct BooleanExpression {
  pub value: bool,
}

impl fmt::Debug for BooleanExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(&self.value.to_string())
  }
}

// Call Expression

#[derive(Clone)]
pub enum CallExpression {
  // An immediately invoked function, e.g. fn(a, b) { a + b }(2, 2)
  Immediate {
    arguments: Vec<Expression>,
    fn_lit: FunctionLiteral,
  },
  // Invocation of a function bound to an ident,
  // e.g. let foo = fn(a, b) { a + b }; foo(2, 2);
  Named {
    arguments: Vec<Expression>,
    ident: IdentExpression,
  },
}

impl fmt::Debug for CallExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let (name, arguments) = match self {
      CallExpression::Immediate { arguments, .. } => {
        (String::from("[anonymous]"), arguments)
      },
      CallExpression::Named { arguments, ident } => {
        (format!("{:?}", ident), arguments)
      },
    };

    write!(
      f,
      "{}({})",
      name,
      arguments
        .iter()
        .map(|arg| format!("{:?}", arg))
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

// Ident Expression

#[derive(Clone)]
pub struct IdentExpression {
  pub value: String,
}

impl fmt::Debug for IdentExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(&self.value)
  }
}

// If Expression

#[derive(Clone)]
pub struct IfExpression {
  pub alternative: Option<BlockStatement>,
  pub condition: Box<Expression>,
  pub consequence: BlockStatement,
}

impl fmt::Debug for IfExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self.alternative {
      Some(alt) => {
        write!(
          f,
          "if ({:?}) {:?} else {:?}",
          self.condition, self.consequence, alt
        )
      },
      _ => write!(f, "if ({:?}) {:?}", self.condition, self.consequence),
    }
  }
}

// Index Expression

#[derive(Clone)]
pub struct IndexExpression {
  pub index: Box<Expression>,
  pub operand: Box<Expression>,
}

impl fmt::Debug for IndexExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}[{:?}]", self.operand, self.index)
  }
}

// Infix Expression

#[derive(Clone)]
pub struct InfixExpression {
  pub left_operand: Box<Node>,
  pub operator: Operator,
  pub right_operand: Box<Node>,
}

impl fmt::Debug for InfixExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "{:?} {} {:?}",
      self.left_operand, self.operator, self.right_operand
    )
  }
}

// Prefix Expression

#[derive(Clone)]
pub struct PrefixExpression {
  pub operator: Operator,
  pub operand: Box<Node>,
}

impl fmt::Debug for PrefixExpression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} {:?}", self.operator, self.operand)
  }
}

// -- Literals ----------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Literal {
  Array(ArrayLiteral),
  Function(FunctionLiteral),
  Hash(HashLiteral),
  Number(NumberLiteral),
  String(StringLiteral),
}

impl From<ArrayLiteral> for Literal {
  fn from(array_lit: ArrayLiteral) -> Self {
    Literal::Array(array_lit)
  }
}

impl From<FunctionLiteral> for Literal {
  fn from(fn_lit: FunctionLiteral) -> Self {
    Literal::Function(fn_lit)
  }
}

impl From<HashLiteral> for Literal {
  fn from(hash_lit: HashLiteral) -> Self {
    Literal::Hash(hash_lit)
  }
}

impl From<NumberLiteral> for Literal {
  fn from(num_lit: NumberLiteral) -> Self {
    Literal::Number(num_lit)
  }
}

impl From<StringLiteral> for Literal {
  fn from(str_lit: StringLiteral) -> Self {
    Literal::String(str_lit)
  }
}

// Array Literal

#[derive(Clone)]
pub struct ArrayLiteral {
  pub elements: Vec<Expression>,
}

impl fmt::Debug for ArrayLiteral {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "[{}]",
      self
        .elements
        .iter()
        .map(|element| format!("{:?}", element))
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

// Function Literal

#[derive(Clone)]
pub struct FunctionLiteral {
  pub body: BlockStatement,
  pub parameters: Vec<IdentExpression>,
}

impl fmt::Debug for FunctionLiteral {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "fn({}) {:?}",
      self
        .parameters
        .iter()
        .map(|parameter| format!("{:?}", parameter))
        .collect::<Vec<_>>()
        .join(", "),
      self.body
    )
  }
}

// Hash Literal

#[derive(Clone)]
pub struct HashLiteral {
  pub pairs: Vec<(Expression, Expression)>,
}

impl fmt::Debug for HashLiteral {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let formatted_pairs: Vec<_> = self
      .pairs
      .iter()
      .map(|(key, value)| format!("{:?}: {:?}", key, value))
      .collect();

    write!(f, "{{ {} }}", formatted_pairs.join(", "))
  }
}

// Number Literal

#[derive(Clone)]
pub struct NumberLiteral {
  pub value: i64,
}

impl fmt::Debug for NumberLiteral {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(&self.value.to_string())
  }
}

// String Literal

#[derive(Clone)]
pub struct StringLiteral {
  pub value: String,
}

impl fmt::Debug for StringLiteral {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(&self.value)
  }
}

// -- Statements --------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Statement {
  #[allow(dead_code)]
  Block(BlockStatement),
  Expression(ExpressionStatement),
  Let(LetStatement),
  Return(ReturnStatement),
}

impl From<ExpressionStatement> for Statement {
  fn from(expr_stmt: ExpressionStatement) -> Self {
    Statement::Expression(expr_stmt)
  }
}

impl From<LetStatement> for Statement {
  fn from(let_stmt: LetStatement) -> Self {
    Statement::Let(let_stmt)
  }
}

impl From<ReturnStatement> for Statement {
  fn from(return_stmt: ReturnStatement) -> Self {
    Statement::Return(return_stmt)
  }
}

// Block Statement

#[derive(Clone)]
pub struct BlockStatement {
  pub statements: Vec<Statement>,
}

impl fmt::Debug for BlockStatement {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "{{ {} }}",
      self
        .statements
        .iter()
        .map(|statement| format!("{:?}", statement))
        .collect::<Vec<_>>()
        .join("")
    )
  }
}

// Expression Statement

#[derive(Clone)]
pub struct ExpressionStatement {
  pub expression: Expression,
}

impl fmt::Debug for ExpressionStatement {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self.expression)
  }
}

// Let Statement

#[derive(Clone)]
pub struct LetStatement {
  pub identifier: IdentExpression,
  pub expression: Expression,
}

impl fmt::Debug for LetStatement {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "let {:?} = {:?}", self.identifier, self.expression)
  }
}

// Return Statement

#[derive(Clone)]
pub struct ReturnStatement {
  pub expression: Expression,
}

impl fmt::Debug for ReturnStatement {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "return {:?}", self.expression)
  }
}
