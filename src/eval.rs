use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;

use crate::ast::{
  ArrayLiteral,
  BlockStatement,
  BooleanExpression,
  CallExpression,
  Expression,
  ExpressionStatement,
  FunctionLiteral,
  HashLiteral,
  IdentExpression,
  IfExpression,
  IndexExpression,
  InfixExpression,
  LetStatement,
  Literal,
  Node,
  NumberLiteral,
  PrefixExpression,
  ReturnStatement,
  Statement,
  StringLiteral,
};
use crate::built_in::{BuiltInError, BUILT_IN_FUNCTIONS};
use crate::object::{
  MonkeyArray,
  MonkeyBool,
  MonkeyFunction,
  MonkeyHash,
  MonkeyHashKey,
  MonkeyInteger,
  MonkeyNull,
  MonkeyReturn,
  MonkeyString,
  Object,
  ObjectError,
};
use crate::operator::Operator;
use crate::scope::{Scope, ShareableScope};

pub type EvalResult = Result<Object, EvalError>;

/// Traverses and evaluates each node in the given Monkey AST to produce
/// a single computed result in the form of an `Object`.
///
/// # Examples
///
/// ```
/// let monkey_source = r#" "hello, world!" "#;
/// let lexer = Lexer::new(monkey_source).tokens();
/// let program = Parser::new(tokens).parse();
/// let result = eval(program.ast);
/// ```
pub fn eval(ast: Vec<Node>) -> EvalResult {
  let scope = Scope::global(BUILT_IN_FUNCTIONS.to_vec());
  let mut object = Object::Null(MonkeyNull);

  for node in ast {
    object = node.eval(&scope)?;

    if matches!(object, Object::Return(..)) {
      break;
    }
  }

  Ok(object)
}

impl Node {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    match self {
      Node::Expression(expr) => expr.eval(scope),
      Node::Literal(lit) => lit.eval(scope),
      Node::Statement(stmt) => stmt.eval(scope),
    }
  }
}

// -- Expression Eval ---------------------------------------------------------

impl Expression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    match self {
      Expression::Boolean(bool_expr) => bool_expr.eval(),
      Expression::Call(call_expr) => call_expr.eval(scope),
      Expression::Ident(ident_expr) => ident_expr.eval(scope),
      Expression::If(if_expr) => if_expr.eval(scope),
      Expression::Index(index_expr) => index_expr.eval(scope),
      Expression::Infix(infix_expr) => infix_expr.eval(scope),
      Expression::Literal(lit_expr) => lit_expr.eval(scope),
      Expression::Prefix(prefix_expr) => prefix_expr.eval(scope),
    }
  }
}

// Boolean Expression

impl BooleanExpression {
  fn eval(self) -> EvalResult {
    Ok(Object::Bool(MonkeyBool { value: self.value }))
  }
}

// Call Expression

impl CallExpression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let (arguments, func_obj) = match self {
      CallExpression::Immediate { arguments, fn_lit } => (arguments, fn_lit.eval(scope)?),
      CallExpression::Named { arguments, ident } => (arguments, ident.eval(scope)?),
    };

    let arguments = Self::eval_arguments(arguments, scope)?;

    match func_obj {
      Object::BuiltIn(built_in) => (built_in.value.function)(arguments).map_err(EvalError::from),
      Object::Function(func) => {
        let MonkeyFunction {
          body,
          parameters,
          scope: fn_scope,
        } = func;

        arguments
          .into_iter()
          .zip(parameters)
          .for_each(|(arg, param)| {
            fn_scope.borrow_mut().set_binding(param.value, arg);
          });

        body.eval(&fn_scope)
      },
      other_obj => Err(EvalError::ObjectNotCallable(other_obj)),
    }
  }

  fn eval_arguments(
    arguments: Vec<Expression>,
    scope: &ShareableScope,
  ) -> Result<Vec<Object>, EvalError> {
    let mut evaluated_args = Vec::new();

    for arg in arguments.into_iter() {
      evaluated_args.push(arg.eval(scope)?);
    }

    Ok(evaluated_args)
  }
}

// Ident Expression

impl IdentExpression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    scope
      .borrow()
      .get_binding(&self.value)
      .ok_or(EvalError::UnknownIdent(self.value))
  }
}

// If Expression

impl IfExpression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    if self.condition.eval(scope)?.is_truthy() {
      return self.consequence.eval(scope);
    }

    self
      .alternative
      .map_or(Ok(Object::Null(MonkeyNull)), |alt| alt.eval(scope))
  }
}

// Index Expression

impl IndexExpression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let operand_obj = self.operand.eval(scope)?;
    let index_obj = self.index.eval(scope)?;

    operand_obj.index(index_obj).map_err(EvalError::from)
  }
}

// Infix Expression

impl InfixExpression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let left_obj = self.left_operand.eval(scope)?;
    let right_obj = self.right_operand.eval(scope)?;

    match self.operator {
      Operator::Add => (left_obj + right_obj).map_err(EvalError::from),
      Operator::Bang => Err(EvalError::InvalidInfixBang),
      Operator::Divide => (left_obj / right_obj).map_err(EvalError::from),
      Operator::Equal => {
        Ok(Object::Bool(MonkeyBool {
          value: left_obj == right_obj,
        }))
      },
      Operator::GreaterThan => {
        Ok(Object::Bool(MonkeyBool {
          value: left_obj > right_obj,
        }))
      },
      Operator::LessThan => {
        Ok(Object::Bool(MonkeyBool {
          value: left_obj < right_obj,
        }))
      },
      Operator::Multiply => (left_obj * right_obj).map_err(EvalError::from),
      Operator::Negate => Err(EvalError::InvalidInfixNegation),
      Operator::NotEqual => {
        Ok(Object::Bool(MonkeyBool {
          value: left_obj != right_obj,
        }))
      },
      Operator::Subtract => (left_obj - right_obj).map_err(EvalError::from),
    }
  }
}

// Prefix Expression

impl PrefixExpression {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let operand = self.operand.eval(scope)?;

    match self.operator {
      Operator::Bang => {
        Ok(Object::Bool(MonkeyBool {
          value: !operand.is_truthy(),
        }))
      },
      Operator::Negate => (-operand).map_err(EvalError::from),
      Operator::Subtract => {
        match operand {
          Object::Integer(MonkeyInteger { value }) => {
            Ok(Object::Integer(MonkeyInteger { value: -value }))
          },
          obj => Err(EvalError::ObjectNotNegateable(obj)),
        }
      },
      _ => Err(EvalError::InvalidPrefixOperator(self.operator)),
    }
  }
}

// -- Literal Eval ------------------------------------------------------------

impl Literal {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    match self {
      Literal::Array(array_lit) => array_lit.eval(scope),
      Literal::Function(fn_lit) => fn_lit.eval(scope),
      Literal::Hash(hash_lit) => hash_lit.eval(scope),
      Literal::Number(number_lit) => number_lit.eval(),
      Literal::String(string_lit) => string_lit.eval(),
    }
  }
}

// Array Literal

impl ArrayLiteral {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let mut items = Vec::new();

    for element in self.elements.into_iter() {
      items.push(element.eval(scope)?);
    }

    Ok(Object::Array(MonkeyArray { items }))
  }
}

// Function Literal

impl FunctionLiteral {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    Ok(Object::Function(MonkeyFunction {
      body: self.body,
      parameters: self.parameters,
      scope: Scope::nested(Rc::clone(scope)),
    }))
  }
}

// Hash Literal

impl HashLiteral {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let mut map = HashMap::new();

    for (key, value) in self.pairs {
      let hash_key = MonkeyHashKey::try_from(key.eval(scope)?)?;
      let hash_value = value.eval(scope)?;

      map.insert(hash_key, hash_value);
    }

    Ok(Object::Hash(MonkeyHash { map }))
  }
}

// Number Literal

impl NumberLiteral {
  fn eval(self) -> EvalResult {
    Ok(Object::Integer(MonkeyInteger { value: self.value }))
  }
}

// String Literal

impl StringLiteral {
  fn eval(self) -> EvalResult {
    Ok(Object::String(MonkeyString { value: self.value }))
  }
}

// -- Statement Eval ----------------------------------------------------------

impl Statement {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    match self {
      Statement::Block(block_stmt) => block_stmt.eval(scope),
      Statement::Expression(expr_stmt) => expr_stmt.eval(scope),
      Statement::Let(let_stmt) => let_stmt.eval(scope),
      Statement::Return(ret_stmt) => ret_stmt.eval(scope),
    }
  }
}

// Block Statement

impl BlockStatement {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let mut object = Object::Null(MonkeyNull);

    for statement in self.statements {
      object = statement.eval(scope)?;

      if matches!(object, Object::Return(..)) {
        return Ok(object);
      }
    }

    Ok(object)
  }
}

// Expression Statement

impl ExpressionStatement {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    self.expression.eval(scope)
  }
}

// Let Statement

impl LetStatement {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    let obj = self.expression.eval(scope)?;

    scope
      .borrow_mut()
      .set_binding(self.identifier.value, obj.clone());

    Ok(obj)
  }
}

// Return Statement

impl ReturnStatement {
  fn eval(self, scope: &ShareableScope) -> EvalResult {
    Ok(Object::Return(MonkeyReturn {
      value: Box::new(self.expression.eval(scope)?),
    }))
  }
}

// -- Errors ------------------------------------------------------------------

pub enum EvalError {
  BuiltInError(BuiltInError),
  InvalidInfixBang,
  InvalidInfixNegation,
  InvalidPrefixOperator(Operator),
  ObjectError(ObjectError),
  ObjectNotCallable(Object),
  ObjectNotNegateable(Object),
  UnknownIdent(String),
}

impl From<ObjectError> for EvalError {
  fn from(obj_err: ObjectError) -> Self {
    EvalError::ObjectError(obj_err)
  }
}

impl From<BuiltInError> for EvalError {
  fn from(built_in_err: BuiltInError) -> Self {
    EvalError::BuiltInError(built_in_err)
  }
}

impl fmt::Display for EvalError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      EvalError::BuiltInError(err) => {
        write!(f, "{}", err)
      },
      EvalError::InvalidInfixBang => {
        write!(f, "cannot use `!` as an infix operator")
      },
      EvalError::InvalidInfixNegation => {
        write!(f, "cannot use `-` (negate) as an infix operator")
      },
      EvalError::InvalidPrefixOperator(operator) => {
        write!(f, "cannot use `{}` as a prefix operator", operator)
      },
      EvalError::ObjectError(err) => {
        write!(f, "{}", err)
      },
      EvalError::ObjectNotCallable(obj) => {
        write!(f, "`{:?}` is not callable, expected a function", obj)
      },
      EvalError::ObjectNotNegateable(obj) => {
        write!(f, "`{:?}` is not negateable, expected an integer", obj)
      },
      EvalError::UnknownIdent(ident) => {
        write!(f, "unknown identifier `{:?}`", ident)
      },
    }
  }
}

// -- Tests -------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;
  use crate::parser::Parser;

  macro_rules! eval_test {
    ($input:tt => $expected_output:tt) => {{
      {
        let tokens = Lexer::new($input).tokens();
        let program = Parser::new(tokens).parse();

        if !program.errors.is_empty() {
          panic!("Encountered errors during parsing: {:#?}", program.errors);
        }

        let output = match eval(program.ast) {
          Ok(obj) => format!("{}", obj),
          Err(eval_err) => panic!("Encountered errors during eval: {}", eval_err),
        };

        assert_eq!(output, $expected_output);
      }
    }};

    ($input:tt => $expected_output:tt, $($more_input:tt => $more_expected_output:tt),+) => {{
      {
        eval_test! { $input => $expected_output };
        eval_test! { $($more_input => $more_expected_output),+ };
      }
    }};
  }

  #[test]
  fn test_evaluating_integer_expressions() {
    eval_test! {
      "5" => "5",
      "10" => "10",
      "-5" => "-5",
      "-10" => "-10",
      "5 + 5 + 5 + 5 - 10" => "10",
      "2 * 2 * 2 * 2 * 2" => "32",
      "-50 + 100 + -50" => "0",
      "5 * 2 + 10" => "20",
      "5 + 2 * 10" => "25",
      "20 + 2 * -10" => "0",
      "50 / 2 * 2 + 10" => "60",
      "2 * (5 + 10)" => "30",
      "3 * 3 * 3 + 10" => "37",
      "3 * (3 * 3) + 10" => "37",
      "(5 + 10 * 2 + 15 / 3) * 2 + -10" => "50",
      "-5 + 5" => "0"
    }
  }

  #[test]
  fn test_evaluating_boolean_expressions() {
    eval_test! {
      "true" => "true",
      "false" => "false",
      "1 < 2" => "true",
      "1 > 2" => "false",
      "1 < 1" => "false",
      "1 == 1" => "true",
      "1 != 1" => "false",
      "true == true" => "true",
      "false == false" => "true",
      "true == false" => "false",
      "true != false" => "true",
      "false != true" => "true",
      "(1 < 2) == true" => "true",
      "(1 < 2) == false" => "false",
      "(1 > 2) == true" => "false",
      "(1 > 2) == false" => "true"
    }
  }

  #[test]
  fn test_evaluating_bang_operator() {
    eval_test! {
      "!true" => "false",
      "!false" => "true",
      "!5" => "false",
      "!!true" => "true",
      "!!false" => "false",
      "!!5" => "true"
    }
  }

  #[test]
  fn test_evaluating_if_else_expressions() {
    eval_test! {
      "if (true) { 10 }" => "10",
      "if (false) { 10 }" => "[null]",
      "if (1) { 10 }" => "10",
      "if (1 < 2) { 10 }" => "10",
      "if (1 > 2) { 10 }" => "[null]",
      "if (1 > 2) { 10 } else { 20 }" => "20",
      "if (1 < 2) { 10 } else { 20 }" => "10"
    }
  }

  #[test]
  fn test_evaluating_let_statements() {
    eval_test! {
      "let a = 5; a;" => "5",
      "let a = 5 * 5; a;" => "25",
      "let a = 5; let b = a; b;" => "5",
      "let a = 5; let b = a; let c = a + b + 5; c;" => "15"
    }
  }

  #[test]
  fn test_function_application() {
    eval_test! {
      "
        let identity = fn(x) { x; };
        identity(5)
      " => "5",

      "
        let double = fn(x) {
          x * 2
        };
        
        double(5)
      " => "10",

      "
        let add = fn(x, y) {
          x + y;
        };
        
        add(5, 5)
      " => "10",

      "
        let add = fn(x, y) {
          x + y;
        };
        
        add(5 + 5, add(5, 5))
      " => "20",

      "fn(a, b) { a + b }(2, 2)" => "4"
    }
  }

  #[test]
  fn test_evaluating_return_statements() {
    eval_test! {
      "return 10" => "10",

      "
        return 10;
        9;
      " => "10",

      "
        return 2 * 5;
        9;
      " => "10",

      "
        9;
        return 2 * 5;
        9;
      " => "10",

      "
        if (10 > 1) {
          return 10;
        }
      " => "10",

      "
        if (10 > 1) {
          if (10 > 1) {
            return 10;
          }
          
          return 1;
        }
      " => "10",

      "
        let f = fn(x) {
          return x;
          x + 10;
        };
        
        f(10);
      " => "10",

      "
        let f = fn(x) {
          let result = x + 10;
          return result;
          return 10; 
        };
        
        f(10)
      " => "20"
    }
  }

  #[test]
  fn test_scope() {
    eval_test! {
      "
        let first = 10;
        let second = 10;
        let third = 10;
        
        let foo = fn(first) {
          let second = 20;
        
          first + second + third;
        };
        
        foo(20) + first + second;
      " => "70"
    }
  }

  #[test]
  fn test_closure() {
    eval_test! {
      "
        let makeAdder = fn(x) {
          fn(y) { x + y }
        }
        
        let addTwo = makeAdder(2);
        
        addTwo(2);
      " => "4"
    }
  }

  #[test]
  fn test_evaluating_string_literals() {
    eval_test! {
      r#"
        let a = "hello";
        let b = "world";
        a + b
      "# => "helloworld"
    }
  }

  #[test]
  fn test_evaluating_built_in_len() {
    eval_test! {
      r#" len("monkey") "# => "6",
      "len([1, 2, 3])" => "3"
    }
  }

  #[test]
  fn test_evaluating_built_in_first() {
    eval_test! {
      "first([1, 2, 3])" => "1"
    }
  }

  #[test]
  fn test_evaluating_built_in_last() {
    eval_test! {
      "last([1, 2, 3])" => "3"
    }
  }

  #[test]
  fn test_evaluating_built_in_rest() {
    eval_test! {
      "rest([1, 2, 3])" => "(2)[2, 3]"
    }
  }

  #[test]
  fn test_evaluating_built_in_push() {
    eval_test! {
      "
        let arr = [1];
        push(arr, 2)
      " => "(2)[1, 2]"
    }
  }

  #[test]
  fn test_evaluating_array_literal() {
    eval_test! {
      r#" [1, "two", 1 * 3] "# => "(3)[1, two, 3]"
    }
  }

  #[test]
  fn test_evaluating_array_index() {
    eval_test! {
      "[1, 2, 3][0]" => "1",
      "[1, 2, 3][1]" => "2",
      "[1, 2, 3][2]" => "3",
      "let i = 0; [1][i]" => "1",
      "[1, 2, 3][1 + 1]" => "3",

      "
        let myArray = [1, 2, 3];
        myArray[2];
      " => "3",

      "
        let myArray = [1, 2, 3];
        myArray[0] + myArray[1] + myArray[2];
      " => "6",

      "
        let myArray = [1, 2, 3];
        let i = myArray[0];
        myArray[i]
      " => "2",

      "[1, 2, 3][3]" => "[null]",
      "[1, 2, 3][-1]" => "[null]"
    }
  }

  #[test]
  fn test_evaluating_hash_literal() {
    eval_test! {
      r#"
        let two = "two";

        {
          "One": 10 - 9,
          two: 1 + 1,
          "thr" + "ee": 6 / 2,
          4: 4,
          true: 5,
          false: 6
        }
      "# => "{4: 4, false: 6, One: 1, three: 3, true: 5, two: 2}"
    }
  }

  #[test]
  fn test_evaluating_hash_index() {
    eval_test! {
      r#" {"foo": 5}["foo"] "# => "5",
      r#" {"foo": 5}["bar"] "# => "[null]",

      r#"
        let key = "foo";
        {"foo": 5}[key]
      "# => "5",

      r#" {}["foo"] "# => "[null]",
      "{5: 5}[5]" => "5",
      "{true: 5}[true]" => "5",
      "{false: 5}[false]" => "5"
    }
  }
}
