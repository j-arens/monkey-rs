use std::convert::TryFrom;
use std::fmt;
use std::iter::Peekable;
use std::str;
use std::vec::IntoIter;

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
  Node,
  NumberLiteral,
  PrefixExpression,
  ReturnStatement,
  Statement,
  StringLiteral,
};
use crate::lexer::Token;
use crate::operator::{Operator, OperatorError, OperatorPrecedence};

/// The result of parsing. Contains the abstract syntax tree and any errors
/// occured during parsing.
pub struct Program {
  pub ast: Vec<Node>,
  pub errors: Vec<ParseError>,
}

type TokenIter = Peekable<IntoIter<Token>>;

/// A recursive descent parser (Pratt parser) that constructs an abstract
/// syntax tree from the given tokens.
///
/// # Examples
///
/// ```
/// let monkey_source = r#" "hello, world!" "#;
/// let lexer = Lexer::new(monkey_source).tokens();
/// let program = Parser::new(tokens).parse();
/// ```
pub struct Parser {
  token_iter: TokenIter,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self {
      token_iter: tokens.into_iter().peekable(),
    }
  }

  pub fn parse(mut self) -> Program {
    let mut ast = Vec::new();
    let mut errors = Vec::new();
    let tokens = self.token_iter.by_ref();

    while tokens.peek().is_some() {
      match Node::parse(tokens) {
        Ok(node) => ast.push(node),
        Err(err_msg) => {
          errors.push(err_msg);
          tokens.next();
        },
      }
    }

    Program { ast, errors }
  }
}

type ParseResult<T> = Result<T, ParseError>;

// -- Generic Node Parsing ------------------------------------------------

impl Node {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    Ok(Statement::parse(tokens)?.into())
  }
}

// -- Expression Parsing ------------------------------------------------------

// Expression

impl Expression {
  fn parse(tokens: &mut TokenIter, op_precedence: OperatorPrecedence) -> ParseResult<Self> {
    let expr = Expression::parse_left(tokens)?;
    let expr = Expression::parse_right(tokens, expr, op_precedence)?;
    Ok(expr)
  }

  fn parse_left(tokens: &mut TokenIter) -> ParseResult<Self> {
    match tokens.peek() {
      Some(Token::Ident { .. }) => Ok(IdentExpression::parse(tokens)?.into()),
      Some(Token::If) => Ok(IfExpression::parse(tokens)?.into()),
      Some(Token::Number { .. }) => Ok(NumberLiteral::parse(tokens)?.into()),
      Some(Token::Bang) | Some(Token::Minus) => Ok(PrefixExpression::parse(tokens)?.into()),
      Some(Token::True) | Some(Token::False) => Ok(BooleanExpression::parse(tokens)?.into()),
      Some(Token::LeftParen) => Ok(Expression::parse_group(tokens)?),
      Some(Token::Function) => Ok(FunctionLiteral::parse(tokens)?.into()),
      Some(Token::String { .. }) => Ok(StringLiteral::parse(tokens)?.into()),
      Some(Token::LeftBracket) => Ok(ArrayLiteral::parse(tokens)?.into()),
      Some(Token::LeftBrace) => Ok(HashLiteral::parse(tokens)?.into()),
      Some(peeked_token) => Err(ParseError::UnknownExpression(peeked_token.clone())),
      None => Err(ParseError::EmptyExpression),
    }
  }

  fn parse_right(
    tokens: &mut TokenIter,
    left_expr: Self,
    op_precedence: OperatorPrecedence,
  ) -> ParseResult<Self> {
    let next_token = tokens.peek();

    if next_token.is_none() {
      return Ok(left_expr);
    }

    let next_token = next_token.unwrap();

    if next_token == &Token::Semicolon {
      return Ok(left_expr);
    }

    if OperatorPrecedence::from(next_token) <= op_precedence {
      return Ok(left_expr);
    }

    match next_token {
      Token::Asterisk
      | Token::Plus
      | Token::Minus
      | Token::Equal
      | Token::NotEqual
      | Token::LessThan
      | Token::GreaterThan
      | Token::Slash => {
        let infix_expr = InfixExpression::parse(tokens, left_expr)?;
        Expression::parse_right(tokens, infix_expr.into(), op_precedence)
      },
      Token::LeftParen => {
        match left_expr {
          Expression::Ident(ident_expr) => {
            let call_expr = CallExpression::parse(tokens, ident_expr)?;
            Expression::parse_right(tokens, call_expr.into(), op_precedence)
          },
          _ => Err(ParseError::InvalidInvocation),
        }
      },
      Token::LeftBracket => {
        let index_expr = IndexExpression::parse(tokens, left_expr)?;
        Expression::parse_right(tokens, index_expr.into(), op_precedence)
      },
      _ => Ok(left_expr),
    }
  }

  fn parse_group(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::LeftParen)
      .ok_or(ParseError::InvalidGroupedExpressionOpening)?;

    let expr = Expression::parse(tokens, OperatorPrecedence::Base)?;

    tokens
      .next_if_eq(&Token::RightParen)
      .ok_or(ParseError::InvalidGroupedExpressionClosing)?;

    Ok(expr)
  }

  fn parse_list(
    tokens: &mut TokenIter,
    ending_delimiter: Token,
  ) -> Result<Vec<Expression>, ParseError> {
    let mut list = Vec::new();

    loop {
      match tokens.peek() {
        Some(token) if *token == ending_delimiter => {
          tokens.next();
          break;
        },
        Some(Token::Comma) => {
          tokens.next();
          continue;
        },
        _ => list.push(Expression::parse(tokens, OperatorPrecedence::Base)?),
      }
    }

    Ok(list)
  }
}

// Boolean Expression

impl BooleanExpression {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    match tokens.next() {
      Some(Token::False) => Ok(Self { value: false }),
      Some(Token::True) => Ok(Self { value: true }),
      Some(other_token) => Err(ParseError::InvalidBooleanExpression(other_token)),
      None => Err(ParseError::UnexpectedEndOfTokens),
    }
  }
}

// Call Expression

impl CallExpression {
  fn parse(tokens: &mut TokenIter, ident: IdentExpression) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::LeftParen)
      .ok_or(ParseError::InvalidCallExpressionOpening)?;

    let arguments = Expression::parse_list(tokens, Token::RightParen)?;

    // Advance optional semicolon.
    tokens.next_if_eq(&Token::Semicolon);

    Ok(CallExpression { arguments, ident })
  }
}

// Ident Expression

impl IdentExpression {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    match tokens.next() {
      Some(Token::Ident { bytes }) => {
        if let Ok(ascii) = str::from_utf8(&bytes[..]) {
          return Ok(Self {
            value: ascii.to_owned(),
          });
        }

        Err(ParseError::ParseIdentFailure(bytes))
      },
      Some(other_token) => Err(ParseError::InvalidIdent(other_token)),
      None => Err(ParseError::UnexpectedEndOfTokens),
    }
  }
}

// If Expression

impl IfExpression {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::If)
      .ok_or(ParseError::InvalidIfExpressionOpening)?;

    tokens
      .next_if_eq(&Token::LeftParen)
      .ok_or(ParseError::InvalidIfConditionOpening)?;

    let condition = Expression::parse(tokens, OperatorPrecedence::Base)?;

    tokens
      .next_if_eq(&Token::RightParen)
      .ok_or(ParseError::InvalidIfConditionClosing)?;

    let consequence = BlockStatement::parse(tokens)?;

    match tokens.peek() {
      Some(Token::Else) => {
        tokens.next();

        let alt = BlockStatement::parse(tokens)?;

        Ok(IfExpression {
          alternative: Some(alt),
          condition: Box::new(condition),
          consequence,
        })
      },
      _ => {
        Ok(IfExpression {
          alternative: None,
          condition: Box::new(condition),
          consequence,
        })
      },
    }
  }
}

// Index Expression

impl IndexExpression {
  fn parse(tokens: &mut TokenIter, left_expr: Expression) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::LeftBracket)
      .ok_or(ParseError::InvalidIndexExpressionOpening)?;

    if matches!(tokens.peek(), Some(&Token::RightBracket)) {
      return Err(ParseError::EmptyIndexExpression);
    }

    let index_expr = IndexExpression {
      index: Box::new(Expression::parse(tokens, OperatorPrecedence::Base)?),
      operand: Box::new(left_expr),
    };

    tokens
      .next_if_eq(&Token::RightBracket)
      .ok_or(ParseError::InvalidIndexExpressionClosing)?;

    Ok(index_expr)
  }
}

// Infix Expression

impl InfixExpression {
  fn parse(tokens: &mut TokenIter, left_expr: Expression) -> ParseResult<Self> {
    let op_token = tokens.next();

    if op_token.is_none() {
      return Err(ParseError::UnexpectedEndOfTokens);
    }

    let op_token = op_token.unwrap();
    let operator = Operator::try_from(&op_token)?;
    let op_precedence = OperatorPrecedence::from(&op_token);
    let right_expr = Expression::parse(tokens, op_precedence)?;

    Ok(Self {
      left_operand: Box::new(left_expr.into()),
      operator,
      right_operand: Box::new(right_expr.into()),
    })
  }
}

// Prefix Expression

impl PrefixExpression {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    let op_token = tokens.next();

    if op_token.is_none() {
      return Err(ParseError::UnexpectedEndOfTokens);
    }

    let op_token = op_token.unwrap();
    let operator = Operator::try_from(&op_token)?;
    let expr = Expression::parse(tokens, OperatorPrecedence::Prefix)?;

    Ok(Self {
      operator,
      operand: Box::new(expr.into()),
    })
  }
}

// -- Literal Parsing ---------------------------------------------------------

// Array Literal

impl ArrayLiteral {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::LeftBracket)
      .ok_or(ParseError::InvalidArrayLiteralOpening)?;

    let elements = Expression::parse_list(tokens, Token::RightBracket)?;

    Ok(Self { elements })
  }
}

// Function Literal

impl FunctionLiteral {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::Function)
      .ok_or(ParseError::InvalidFunctionLiteralOpening)?;

    tokens
      .next_if_eq(&Token::LeftParen)
      .ok_or(ParseError::InvalidFunctionLiteralParamsOpening)?;

    let mut parameters = Vec::new();

    loop {
      match tokens.peek() {
        Some(Token::RightParen) => {
          tokens.next();
          break;
        },
        Some(Token::Comma) => {
          tokens.next();
          continue;
        },
        Some(Token::Ident { .. }) => parameters.push(IdentExpression::parse(tokens)?),
        Some(other_token) => {
          return Err(ParseError::InvalidFunctionParam(other_token.clone()));
        },
        None => {
          return Err(ParseError::UnexpectedEndOfTokens);
        },
      }
    }

    let body = BlockStatement::parse(tokens)?;

    Ok(Self { body, parameters })
  }
}

// Hash Literal

impl HashLiteral {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::LeftBrace)
      .ok_or(ParseError::InvalidHashLiteralOpening)?;

    let mut pairs = Vec::new();

    loop {
      match tokens.peek() {
        Some(Token::RightBrace) => {
          tokens.next();
          break;
        },
        Some(Token::Comma) => {
          tokens.next();
          continue;
        },
        _ => {
          pairs.push(HashLiteral::parse_pair(tokens)?);
        },
      }
    }

    Ok(HashLiteral { pairs })
  }

  fn parse_pair(tokens: &mut TokenIter) -> Result<(Expression, Expression), ParseError> {
    let key = Expression::parse(tokens, OperatorPrecedence::Base)?;

    tokens
      .next_if_eq(&Token::Colon)
      .ok_or(ParseError::InvalidHashLiteralDelimiter)?;

    let value = Expression::parse(tokens, OperatorPrecedence::Base)?;

    Ok((key, value))
  }
}

// Number Literal

impl NumberLiteral {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    match tokens.next() {
      Some(Token::Number { bytes }) => {
        if let Ok(ascii) = str::from_utf8(&bytes[..]) {
          if let Ok(parsed_number) = ascii.parse() {
            return Ok(Self {
              value: parsed_number,
            });
          }
        }

        Err(ParseError::ParseNumberFailure(bytes))
      },
      Some(other_token) => Err(ParseError::InvalidNumber(other_token)),
      None => Err(ParseError::UnexpectedEndOfTokens),
    }
  }
}

// String Literal

impl StringLiteral {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    match tokens.next() {
      Some(Token::String { bytes }) => {
        if let Ok(string) = str::from_utf8(&bytes[..]) {
          return Ok(Self {
            value: string.to_owned(),
          });
        }

        Err(ParseError::ParseStringFailure(bytes))
      },
      Some(other_token) => Err(ParseError::InvalidString(other_token)),
      None => Err(ParseError::UnexpectedEndOfTokens),
    }
  }
}

// -- Statement Parsing -------------------------------------------------------

impl Statement {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    match tokens.peek() {
      Some(Token::Let) => Ok(LetStatement::parse(tokens)?.into()),
      Some(Token::Return) => Ok(ReturnStatement::parse(tokens)?.into()),
      _ => Ok(ExpressionStatement::parse(tokens)?.into()),
    }
  }
}

// Block Statement

impl BlockStatement {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::LeftBrace)
      .ok_or(ParseError::InvalidBlockStatementOpening)?;

    let mut statements = Vec::new();

    loop {
      match tokens.peek() {
        None => break,
        Some(Token::RightBrace) => {
          tokens.next();
          break;
        },
        _ => {
          statements.push(Statement::parse(tokens)?);
        },
      }
    }

    Ok(Self { statements })
  }
}

// Expression Statement

impl ExpressionStatement {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    let expr_stmt = Self {
      expression: Expression::parse(tokens, OperatorPrecedence::Base)?,
    };

    // Consume optional semicolon.
    tokens.next_if_eq(&Token::Semicolon);

    Ok(expr_stmt)
  }
}

// Let Statement

impl LetStatement {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::Let)
      .ok_or(ParseError::InvalidLetStatementOpening)?;

    let ident = IdentExpression::parse(tokens)?;

    tokens
      .next_if_eq(&Token::Assign)
      .ok_or(ParseError::InvalidLetStatementAssignment)?;

    let expr = Expression::parse(tokens, OperatorPrecedence::Base)?;

    // Consume optional semicolon.
    tokens.next_if_eq(&Token::Semicolon);

    Ok(Self {
      expression: expr,
      identifier: ident,
    })
  }
}

// Return Statement

impl ReturnStatement {
  fn parse(tokens: &mut TokenIter) -> ParseResult<Self> {
    tokens
      .next_if_eq(&Token::Return)
      .ok_or(ParseError::InvalidReturnStatementOpening)?;

    let expr = Expression::parse(tokens, OperatorPrecedence::Base)?;

    // Consume optional semicolon.
    tokens.next_if_eq(&Token::Semicolon);

    Ok(Self { expression: expr })
  }
}

// -- Errors ------------------------------------------------------------------

#[derive(Debug)]
pub enum ParseError {
  EmptyExpression,
  EmptyIndexExpression,
  InvalidArrayLiteralOpening,
  InvalidBlockStatementOpening,
  InvalidBooleanExpression(Token),
  InvalidCallExpressionOpening,
  InvalidFunctionLiteralOpening,
  InvalidFunctionLiteralParamsOpening,
  InvalidFunctionParam(Token),
  InvalidGroupedExpressionClosing,
  InvalidGroupedExpressionOpening,
  InvalidHashLiteralDelimiter,
  InvalidHashLiteralOpening,
  InvalidIdent(Token),
  InvalidIfConditionClosing,
  InvalidIfConditionOpening,
  InvalidIfExpressionOpening,
  InvalidIndexExpressionClosing,
  InvalidIndexExpressionOpening,
  InvalidInvocation,
  InvalidLetStatementAssignment,
  InvalidLetStatementOpening,
  InvalidNumber(Token),
  InvalidReturnStatementOpening,
  InvalidString(Token),
  OperatorError(OperatorError),
  ParseIdentFailure(Vec<u8>),
  ParseNumberFailure(Vec<u8>),
  ParseStringFailure(Vec<u8>),
  UnexpectedEndOfTokens,
  UnknownExpression(Token),
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ParseError::EmptyExpression => {
        write!(f, "cannot parse empty expression")
      },
      ParseError::EmptyIndexExpression => {
        write!(
          f,
          "expected index operation to contain an integer, but found nothing"
        )
      },
      ParseError::InvalidArrayLiteralOpening => {
        write!(
          f,
          "expected array literal to begin with {:?}",
          Token::LeftBracket
        )
      },
      ParseError::InvalidBlockStatementOpening => {
        write!(
          f,
          "expected block statement to begin with {:?}",
          Token::LeftBrace
        )
      },
      ParseError::InvalidBooleanExpression(token) => {
        write!(f, "could not coerce {:?} into a boolean", token)
      },
      ParseError::InvalidCallExpressionOpening => {
        write!(
          f,
          "expected call expression to begin with {:?}",
          Token::LeftParen
        )
      },
      ParseError::InvalidFunctionLiteralOpening => {
        write!(
          f,
          "expected function literal to begin with {:?}",
          Token::Function
        )
      },
      ParseError::InvalidFunctionLiteralParamsOpening => {
        write!(
          f,
          "expected function literl parameters list to begin with {:?}",
          Token::LeftParen
        )
      },
      ParseError::InvalidFunctionParam(token) => {
        write!(f, "unexpected token `{:?}` in function parameters", token)
      },
      ParseError::InvalidGroupedExpressionClosing => {
        write!(
          f,
          "expected grouped expression to end with {:?}",
          Token::RightParen
        )
      },
      ParseError::InvalidGroupedExpressionOpening => {
        write!(
          f,
          "expected grouped expression to begin with {:?}",
          Token::LeftParen
        )
      },
      ParseError::InvalidHashLiteralDelimiter => {
        write!(
          f,
          "expected hash literal keys and values to be delimited by {:?}",
          Token::Colon
        )
      },
      ParseError::InvalidHashLiteralOpening => {
        write!(f, "expected hash literal to begin {:?}", Token::LeftBracket)
      },
      ParseError::InvalidIdent(token) => {
        write!(f, "invalid ident {:?}", token)
      },
      ParseError::InvalidIfConditionClosing => {
        write!(
          f,
          "expected if condition to end with {:?}",
          Token::RightParen
        )
      },
      ParseError::InvalidIfConditionOpening => {
        write!(
          f,
          "expected if condition to begin with {:?}",
          Token::LeftParen
        )
      },
      ParseError::InvalidIfExpressionOpening => {
        write!(f, "expected if expression to begin with `{:?}`", Token::If)
      },
      ParseError::InvalidIndexExpressionClosing => {
        write!(
          f,
          "expected index expression to end with `{:?}`",
          Token::RightBracket
        )
      },
      ParseError::InvalidIndexExpressionOpening => {
        write!(
          f,
          "expected index expression to begin with `{:?}`",
          Token::LeftBracket
        )
      },
      ParseError::InvalidInvocation => {
        write!(f, "expected function call to be preceded by function name")
      },
      ParseError::InvalidLetStatementAssignment => {
        write!(
          f,
          "expected `{:?}` after `{:?}` keyword",
          Token::Assign,
          Token::Let
        )
      },
      ParseError::InvalidLetStatementOpening => {
        write!(
          f,
          "expected let statement to begin with `{:?}` keyword",
          Token::Let
        )
      },
      ParseError::InvalidNumber(token) => {
        write!(f, "invalid number `{:?}`", token)
      },
      ParseError::InvalidReturnStatementOpening => {
        write!(
          f,
          "expected return state to begin with `{:?}` keyword",
          Token::Return
        )
      },
      ParseError::InvalidString(token) => {
        write!(f, "invalid string `{:?}`", token)
      },
      ParseError::OperatorError(op_err) => {
        write!(f, "{}", op_err)
      },
      ParseError::ParseIdentFailure(bytes) => {
        write!(f, "failed to parse identifier `{:?}`", bytes)
      },
      ParseError::ParseNumberFailure(bytes) => {
        write!(f, "failed to parse number `{:?}`", bytes)
      },
      ParseError::ParseStringFailure(bytes) => {
        write!(f, "failed to parse string `{:?}`", bytes)
      },
      ParseError::UnexpectedEndOfTokens => {
        write!(f, "unexpectedly reached end of tokens while parsing")
      },
      ParseError::UnknownExpression(token) => {
        write!(
          f,
          "unexpected token `{:?}` at beginning of expression",
          token
        )
      },
    }
  }
}

impl From<OperatorError> for ParseError {
  fn from(op_err: OperatorError) -> Self {
    ParseError::OperatorError(op_err)
  }
}

// -- Tests -------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;

  // Takes a raw monkey AST string and turns it into a vector of `Statements`,
  // or in other words, a monkey AST in code.
  fn format_ast_str(raw_ast: &str) -> Vec<String> {
    let mut ast = Vec::new();
    let mut statement = String::new();

    for line in raw_ast.lines() {
      let mut trimmed_line = line.trim().to_owned();

      if trimmed_line.is_empty() {
        continue;
      }

      if trimmed_line.starts_with("Statement") && !statement.is_empty() {
        ast.push(statement);
        statement = String::new();
      }

      // Add a space to the beginning and end of block statements to match the
      // debug formatting.
      if trimmed_line.ends_with('{') || trimmed_line.ends_with(',') {
        trimmed_line.push(' ');
      }

      if trimmed_line.starts_with('}') {
        trimmed_line.insert(0, ' ');
      }

      statement.push_str(&trimmed_line);
    }

    ast.push(statement);
    ast
  }

  macro_rules! parser_test {
    ($input:tt => $expected_output:tt) => {{
      {
        let tokens = Lexer::new($input).tokens();
        let Program { ast, errors } = Parser::new(tokens).parse();

        assert_eq!(errors.len(), 0, "Encountered parse errors: {:#?}", errors);

        let output_formatted: Vec<_> = ast.iter().map(|node| format!("{:?}", node)).collect();
        let expected_output_formatted = format_ast_str($expected_output);

        output_formatted
          .into_iter()
          .zip(expected_output_formatted)
          .for_each(|(a, b)| assert_eq!(a, b));
      }
    }};

    ($input:tt => $expected_output:tt, $($more_input:tt => $more_expected_output:tt),+) => {{
      {
        parser_test! { $input => $expected_output };
        parser_test! { $($more_input => $more_expected_output),+ };
      }
    }};
  }

  #[test]
  fn test_parsing_ident() {
    parser_test! {
      "foobar;" => "
        Statement(
          Expression(
            Ident(foobar)
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_number_literal() {
    parser_test! {
      "5;" => "
        Statement(
          Expression(
            Literal(
              Number(5)
            )
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_let_statements() {
    parser_test! {
      "let x = 5;" => "
        Statement(
          Let(let x = Literal(Number(5)))
        )
      ",

      "let y = true;" => "
        Statement(
          Let(let y = Boolean(true))
        )
      ",

      "let foobar = y;" => "
        Statement(
          Let(let foobar = Ident(y))
        )
      "
    }
  }

  #[test]
  fn test_parsing_return_statements() {
    parser_test! {
      "return 5" => "
        Statement(
          Return(return Literal(Number(5)))
        )
      ",

      "return true" => "
        Statement(
          Return(return Boolean(true))
        )
      ",

      "return foobar" => "
        Statement(
          Return(return Ident(foobar))
        )
      "
    }
  }

  #[test]
  fn test_parsing_prefix_expressions() {
    parser_test! {
      "!15;" => "
        Statement(
          Expression(
            Prefix(! Expression(Literal(Number(15))))
          )
        )
      ",

      "-15;" => "
        Statement(
          Expression(
            Prefix(- Expression(Literal(Number(15))))
          )
        )
      ",

      "!foobar;" => "
        Statement(
          Expression(
            Prefix(! Expression(Ident(foobar)))
          )
        )
      ",

      "-foobar;" => "
        Statement(
          Expression(
            Prefix(- Expression(Ident(foobar)))
          )
        )
      ",

      "!true;" => "
        Statement(
          Expression(
            Prefix(! Expression(Boolean(true)))
          )
        )
      ",

      "!false;" => "
        Statement(
          Expression(
            Prefix(! Expression(Boolean(false)))
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_infix_expressions() {
    parser_test! {
      "5 + 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) + Expression(Literal(Number(5))))
          )
        )
      ",

      "5 - 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) - Expression(Literal(Number(5))))
          )
        )
      ",

      "5 * 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) * Expression(Literal(Number(5))))
          )
        )
      ",

      "5 / 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) / Expression(Literal(Number(5))))
          )
        )
      ",

      "5 > 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) > Expression(Literal(Number(5))))
          )
        )
      ",

      "5 < 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) < Expression(Literal(Number(5))))
          )
        )
      ",

      "5 == 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) == Expression(Literal(Number(5))))
          )
        )
      ",

      "5 != 5;" => "
        Statement(
          Expression(
            Infix(Expression(Literal(Number(5))) != Expression(Literal(Number(5))))
          )
        )
      ",

      "foo + bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) + Expression(Ident(bar)))
          )
        )
      ",

      "foo - bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) - Expression(Ident(bar)))
          )
        )
      ",

      "foo * bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) * Expression(Ident(bar)))
          )
        )
      ",

      "foo / bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) / Expression(Ident(bar)))
          )
        )
      ",

      "foo > bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) > Expression(Ident(bar)))
          )
        )
      ",

      "foo < bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) < Expression(Ident(bar)))
          )
        )
      ",

      "foo == bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) == Expression(Ident(bar)))
          )
        )
      ",

      "foo != bar;" => "
        Statement(
          Expression(
            Infix(Expression(Ident(foo)) != Expression(Ident(bar)))
          )
        )
      ",

      "true == true;" => "
        Statement(
          Expression(
            Infix(Expression(Boolean(true)) == Expression(Boolean(true)))
          )
        )
      ",

      "true != false;" => "
        Statement(
          Expression(
            Infix(Expression(Boolean(true)) != Expression(Boolean(false)))
          )
        )
      ",

      "false == false;" => "
        Statement(
          Expression(
            Infix(Expression(Boolean(false)) == Expression(Boolean(false)))
          )
        )
      "
    }
  }

  #[test]
  fn test_operator_precedence() {
    parser_test! {
      "(5 + 10) * 5;" => "
        Statement(
          Expression(
            Infix(
              Expression(
                Infix(
                  Expression(Literal(Number(5))) + Expression(Literal(Number(10))))) * Expression(Literal(Number(5)
                )
              )
            )
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_boolean_expressions() {
    parser_test! {
      "true; false;" => "
        Statement(
          Expression(
            Boolean(true)
          )
        )

        Statement(
          Expression(
            Boolean(false)
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_if_expression() {
    parser_test! {
      "
        if (x < y) {
          x
        }
      " => "
        Statement(
          Expression(
            If(if (Infix(Expression(Ident(x)) < Expression(Ident(y)))) {
              Expression(Ident(x))
            })
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_if_else_expression() {
    parser_test! {
      "
        if (x < y) {
          x
        } else {
          y
        }
      " => "
        Statement(
          Expression(
            If(if (Infix(Expression(Ident(x)) < Expression(Ident(y)))) {
              Expression(Ident(x))
            } else {
              Expression(Ident(y))
            })
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_function_literal() {
    parser_test! {
      "
        fn(x, y) {
          x + y
        }
      " => "
        Statement(
          Expression(
            Literal(
              Function(fn(x, y) {
                Expression(Infix(Expression(Ident(x)) + Expression(Ident(y))))
              })
            )
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_call_expression() {
    parser_test! {
      "add(1, 2 * 3, 4 + 5)" => "
        Statement(
          Expression(
            Call(
              add(
                Literal(Number(1)),
                Infix(Expression(Literal(Number(2))) * Expression(Literal(Number(3)))),
                Infix(Expression(Literal(Number(4))) + Expression(Literal(Number(5))))
              )
            )
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_string_literal() {
    parser_test! {
      r#" "hello, world!" "# => "
        Statement(
          Expression(
            Literal(String(hello, world!))
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_array_literal() {
    parser_test! {
      "[1, 2 * 2, fn(x) { 1 + x }]" => "
        Statement(
          Expression(
            Literal(
              Array([
                Literal(Number(1)),
                Infix(Expression(Literal(Number(2))) * Expression(Literal(Number(2)))),
                Literal(Function(fn(x) {
                  Expression(Infix(Expression(Literal(Number(1))) + Expression(Ident(x))))
                }))
              ])
            )
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_index_expressions() {
    parser_test! {
      "myArray[1 + 1];" => "
        Statement(
          Expression(
            Index(
              Ident(myArray)[Infix(Expression(Literal(Number(1))) + Expression(Literal(Number(1))))]
            )
          )
        )
      ",

      "myArray[0] + myArray[1];" => "
        Statement(
          Expression(
            Infix(
              Expression(
                Index(Ident(myArray)[Literal(Number(0))])) + Expression(Index(Ident(myArray)[Literal(Number(1))])
              )
            )
          )
        )
      ",

      r#" {"foo": 1}["foo"]; "# => "
        Statement(
          Expression(
            Index(Literal(Hash({ Literal(String(foo)): Literal(Number(1)) }))[Literal(String(foo))])
          )
        )
      "
    }
  }

  #[test]
  fn test_index_operator_precedence() {
    parser_test! {
      "add(a * [1, 2, 3, 4][b * c] * d)" => "
        Statement(
          Expression(
            Call(
              add(
                Infix(
                  Expression(
                    Infix(
                      Expression(Ident(a)) * Expression(Index(Literal(Array([
                        Literal(Number(1)),
                        Literal(Number(2)),
                        Literal(Number(3)),
                        Literal(Number(4))
                      ]))[Infix(Expression(Ident(b)) * Expression(Ident(c)))])
                    )
                  )
                ) * Expression(Ident(d)))
              )
            )
          )
        )
      "
    }
  }

  #[test]
  fn test_parsing_hash_literals() {
    parser_test! {
      r#" {"one": 1, "two": 2, "three": 3}; "# => "
        Statement(
          Expression(
            Literal(
              Hash({
                Literal(String(one)): Literal(Number(1)),
                Literal(String(two)): Literal(Number(2)),
                Literal(String(three)): Literal(Number(3))
              })
            )
          )
        )
      ",

      "{};" => "
        Statement(
          Expression(
            Literal(
              Hash({  })
            )
          )
        )
      ",

      r#" {"one": 0 + 1}; "# => "
        Statement(
          Expression(
            Literal(
              Hash({
                Literal(String(one)): Infix(Expression(Literal(Number(0))) + Expression(Literal(Number(1))))
              })
            )
          )
        )
      ",

      "{true: 1, 34: 9};" => "
        Statement(
          Expression(
            Literal(
              Hash({
                Boolean(true): Literal(Number(1)), Literal(Number(34)): Literal(Number(9))
              })
            )
          )
        )
      "
    }
  }
}
