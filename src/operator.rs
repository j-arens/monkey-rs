use std::cmp;
use std::convert::TryFrom;
use std::fmt;

use crate::lexer::Token;

/// All possible infix and prefix operators available in Monkey.
#[derive(Clone)]
pub enum Operator {
  Add,
  Bang,
  Divide,
  Equal,
  GreaterThan,
  LessThan,
  Multiply,
  NotEqual,
  Subtract,
}

impl fmt::Debug for Operator {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(match self {
      Operator::Add => "+",
      Operator::Bang => "!",
      Operator::Divide => "/",
      Operator::Equal => "==",
      Operator::GreaterThan => ">",
      Operator::LessThan => "<",
      Operator::Multiply => "*",
      Operator::NotEqual => "!=",
      Operator::Subtract => "-",
    })
  }
}

// Maps `Token` to an `Operator`.
impl TryFrom<&Token> for Operator {
  type Error = String;

  fn try_from(token: &Token) -> Result<Self, Self::Error> {
    match token {
      Token::Plus => Ok(Operator::Add),
      Token::Minus => Ok(Operator::Subtract),
      Token::Slash => Ok(Operator::Divide),
      Token::Asterisk => Ok(Operator::Multiply),
      Token::Equal => Ok(Operator::Equal),
      Token::NotEqual => Ok(Operator::NotEqual),
      Token::LessThan => Ok(Operator::LessThan),
      Token::GreaterThan => Ok(Operator::GreaterThan),
      Token::Bang => Ok(Operator::Bang),
      _ => Err(format!("Cannot convert {:?} into an `Operator`.", token)),
    }
  }
}

/// All possible precedence "levals" in order from weakest to strongest.
#[derive(Clone, Copy, PartialEq)]
pub enum OperatorPrecedence {
  Base, // 0
  Equality,
  Comparison,
  Sum,
  Product,
  Prefix,
  Call,
  Index,
}

// Maps `Token` to an `OperatorPrecedence`.
impl From<&Token> for OperatorPrecedence {
  fn from(token: &Token) -> Self {
    match token {
      Token::Equal | Token::NotEqual => OperatorPrecedence::Equality,
      Token::GreaterThan | Token::LessThan => OperatorPrecedence::Comparison,
      Token::Minus | Token::Plus => OperatorPrecedence::Sum,
      Token::Asterisk | Token::Slash => OperatorPrecedence::Product,
      Token::LeftParen => OperatorPrecedence::Call,
      Token::LeftBracket => OperatorPrecedence::Index,
      _ => OperatorPrecedence::Base,
    }
  }
}

// Implements <, >, <=, and >= operator overloads for `OperatorPrecedence` in
// Rust.
impl PartialOrd for OperatorPrecedence {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    let a = *self as u8;
    let b = *other as u8;

    if a > b {
      return Some(cmp::Ordering::Greater);
    }

    if a < b {
      return Some(cmp::Ordering::Less);
    }

    Some(cmp::Ordering::Equal)
  }
}
