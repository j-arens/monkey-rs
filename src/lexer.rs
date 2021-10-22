use std::iter::Peekable;
use std::str;

use itertools::Itertools;

/// A Token represents and applies meaning to a known character or sequence of
/// characters from Monkey source code.
#[derive(Debug, PartialEq)]
pub enum Token {
  // Delimiters
  Comma,
  Colon,
  LeftBrace,
  LeftBracket,
  LeftParen,
  RightBrace,
  RightBracket,
  RightParen,
  Semicolon,

  // Keywords
  Else,
  False,
  Function,
  If,
  Let,
  Return,
  True,

  // Literals
  Ident { bytes: Vec<u8> },
  Number { bytes: Vec<u8> },
  String { bytes: Vec<u8> },

  // Operators
  Assign,
  Asterisk,
  Bang,
  Equal,
  GreaterThan,
  LessThan,
  Minus,
  NotEqual,
  Plus,
  Slash,
}

type InputIter<'a> = Peekable<str::Bytes<'a>>;

/// Performs lexical anaylsis of Monkey source code into tokens.
///
/// # Examples
///
/// ```
/// let monkey_source = r#" "hello, world!" "#;
/// let lexer = Lexer::new(monkey_source);
/// let tokens = lexer.tokens();
/// ```
pub struct Lexer<'a> {
  input_iter: InputIter<'a>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      input_iter: input.bytes().peekable(),
    }
  }

  pub fn tokens(mut self) -> Vec<Token> {
    let mut tokens = Vec::new();

    while let Some(token) = self.next_token() {
      tokens.push(token);
    }

    tokens
  }

  fn next_token(&mut self) -> Option<Token> {
    let iter = self.input_iter.by_ref();

    iter.peek()?;

    consume_whitespace(iter);

    consume_keyword_or_ident(iter)
      .or_else(|| consume_symbols(iter))
      .or_else(|| consume_string(iter))
      .or_else(|| consume_digits(iter))
  }
}

/// Peeks at the next byte to determine if it's whitespace. If it is, the byte
/// is consumed.
fn consume_whitespace(iter: &mut InputIter) {
  while iter.next_if(|byte| byte.is_ascii_whitespace()).is_some() {}
}

/// Consumes byte sequences that begin with an ascii alphabet character, and
/// continues to consume until the peeked byte does not match a letter, digit,
/// or underscore. If the consumed byte sequence doesn't match a keyword, it's
/// considered and ident.
fn consume_keyword_or_ident(iter: &mut InputIter) -> Option<Token> {
  let peeked = iter.peek()?;

  // Keywords and idents must begin with a letter.
  if !peeked.is_ascii_alphabetic() {
    return None;
  }

  let bytes: Vec<_> = iter
    .peeking_take_while(|byte| {
      byte.is_ascii_alphabetic()
      || byte.is_ascii_digit() // Allow digits within idents.
      || *byte == b'_' // Allow underscores within idents.
    })
    .collect();

  if bytes.is_empty() {
    return None;
  }

  let token = match bytes[..] {
    [b'e', b'l', b's', b'e'] => Token::Else,
    [b'f', b'a', b'l', b's', b'e'] => Token::False,
    [b'f', b'n'] => Token::Function,
    [b'i', b'f'] => Token::If,
    [b'l', b'e', b't'] => Token::Let,
    [b'r', b'e', b't', b'u', b'r', b'n'] => Token::Return,
    [b't', b'r', b'u', b'e'] => Token::True,
    _ => Token::Ident { bytes },
  };

  Some(token)
}

/// Consumes byte sequences that match certain delimiters or operators.
fn consume_symbols(iter: &mut InputIter) -> Option<Token> {
  match iter.next_if(|byte| byte.is_ascii_punctuation() && *byte != b'"') {
    Some(b'*') => Some(Token::Asterisk),
    Some(b',') => Some(Token::Comma),
    Some(b'>') => Some(Token::GreaterThan),
    Some(b'{') => Some(Token::LeftBrace),
    Some(b'(') => Some(Token::LeftParen),
    Some(b'<') => Some(Token::LessThan),
    Some(b'-') => Some(Token::Minus),
    Some(b'+') => Some(Token::Plus),
    Some(b'}') => Some(Token::RightBrace),
    Some(b')') => Some(Token::RightParen),
    Some(b';') => Some(Token::Semicolon),
    Some(b'/') => Some(Token::Slash),
    Some(b'[') => Some(Token::LeftBracket),
    Some(b']') => Some(Token::RightBracket),
    Some(b':') => Some(Token::Colon),
    Some(b'=') => {
      match iter.next_if(|byte| *byte == b'=') {
        Some(_) => Some(Token::Equal),
        _ => Some(Token::Assign),
      }
    },
    Some(b'!') => {
      match iter.next_if(|byte| *byte == b'=') {
        Some(_) => Some(Token::NotEqual),
        _ => Some(Token::Bang),
      }
    },
    _ => None,
  }
}

/// Consumes byte sequences that begin with double quotes, and the following
/// bytes, up to the next double quote as a string literal.
fn consume_string(iter: &mut InputIter) -> Option<Token> {
  iter.next_if_eq(&b'"')?;

  let bytes: Vec<_> = iter.take_while(|byte| *byte != b'"').collect();

  Some(Token::String { bytes })
}

/// Consumes numeric ascii byte sequences.
fn consume_digits(iter: &mut InputIter) -> Option<Token> {
  let bytes: Vec<_> = iter
    .peeking_take_while(|byte| byte.is_ascii_digit())
    .collect();

  if bytes.is_empty() {
    return None;
  }

  Some(Token::Number { bytes })
}

// -- Tests -------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_tokenization() {
    let input = r#"
let five = 5;
let ten = 10;

!!true;
!!false;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;

let add = fn(x, y) {
  x + y
}

let adder = fn(x) {
  fn (y) {
    x + y
  }
}

"hello, world"
let lang = "monkey";

[1, 2];

{ "foo": "bar" };
  "#;

    let tokens = Lexer::new(input).tokens();

    let expected = vec![
      // let five = 5;
      Token::Let,
      Token::Ident {
        bytes: vec![b'f', b'i', b'v', b'e'],
      },
      Token::Assign,
      Token::Number { bytes: vec![b'5'] },
      Token::Semicolon,
      // let ten = 10;
      Token::Let,
      Token::Ident {
        bytes: vec![b't', b'e', b'n'],
      },
      Token::Assign,
      Token::Number {
        bytes: vec![b'1', b'0'],
      },
      Token::Semicolon,
      // !!true;
      Token::Bang,
      Token::Bang,
      Token::True,
      Token::Semicolon,
      // !!false;
      Token::Bang,
      Token::Bang,
      Token::False,
      Token::Semicolon,
      // let add = fn(x, y) {
      //   x + y;
      // };
      Token::Let,
      Token::Ident {
        bytes: vec![b'a', b'd', b'd'],
      },
      Token::Assign,
      Token::Function,
      Token::LeftParen,
      Token::Ident { bytes: vec![b'x'] },
      Token::Comma,
      Token::Ident { bytes: vec![b'y'] },
      Token::RightParen,
      Token::LeftBrace,
      Token::Ident { bytes: vec![b'x'] },
      Token::Plus,
      Token::Ident { bytes: vec![b'y'] },
      Token::Semicolon,
      Token::RightBrace,
      Token::Semicolon,
      // let result = add(five, ten);
      Token::Let,
      Token::Ident {
        bytes: vec![b'r', b'e', b's', b'u', b'l', b't'],
      },
      Token::Assign,
      Token::Ident {
        bytes: vec![b'a', b'd', b'd'],
      },
      Token::LeftParen,
      Token::Ident {
        bytes: vec![b'f', b'i', b'v', b'e'],
      },
      Token::Comma,
      Token::Ident {
        bytes: vec![b't', b'e', b'n'],
      },
      Token::RightParen,
      Token::Semicolon,
      // !-/*5;
      Token::Bang,
      Token::Minus,
      Token::Slash,
      Token::Asterisk,
      Token::Number { bytes: vec![b'5'] },
      Token::Semicolon,
      // 5 < 10 > 5;
      Token::Number { bytes: vec![b'5'] },
      Token::LessThan,
      Token::Number {
        bytes: vec![b'1', b'0'],
      },
      Token::GreaterThan,
      Token::Number { bytes: vec![b'5'] },
      Token::Semicolon,
      // if (5 < 10) {
      //   return true;
      // } else {
      //   return false;
      // }
      Token::If,
      Token::LeftParen,
      Token::Number { bytes: vec![b'5'] },
      Token::LessThan,
      Token::Number {
        bytes: vec![b'1', b'0'],
      },
      Token::RightParen,
      Token::LeftBrace,
      Token::Return,
      Token::True,
      Token::Semicolon,
      Token::RightBrace,
      Token::Else,
      Token::LeftBrace,
      Token::Return,
      Token::False,
      Token::Semicolon,
      Token::RightBrace,
      // 10 == 10;
      Token::Number {
        bytes: vec![b'1', b'0'],
      },
      Token::Equal,
      Token::Number {
        bytes: vec![b'1', b'0'],
      },
      Token::Semicolon,
      // 10 != 9;
      Token::Number {
        bytes: vec![b'1', b'0'],
      },
      Token::NotEqual,
      Token::Number { bytes: vec![b'9'] },
      Token::Semicolon,
      // let add = fn(x, y) {
      //   x + y
      // }
      Token::Let,
      Token::Ident {
        bytes: vec![b'a', b'd', b'd'],
      },
      Token::Assign,
      Token::Function,
      Token::LeftParen,
      Token::Ident { bytes: vec![b'x'] },
      Token::Comma,
      Token::Ident { bytes: vec![b'y'] },
      Token::RightParen,
      Token::LeftBrace,
      Token::Ident { bytes: vec![b'x'] },
      Token::Plus,
      Token::Ident { bytes: vec![b'y'] },
      Token::RightBrace,
      // let adder = fn(x) {
      //   fn (y) {
      //     x + y
      //   }
      // }
      Token::Let,
      Token::Ident {
        bytes: vec![b'a', b'd', b'd', b'e', b'r'],
      },
      Token::Assign,
      Token::Function,
      Token::LeftParen,
      Token::Ident { bytes: vec![b'x'] },
      Token::RightParen,
      Token::LeftBrace,
      Token::Function,
      Token::LeftParen,
      Token::Ident { bytes: vec![b'y'] },
      Token::RightParen,
      Token::LeftBrace,
      Token::Ident { bytes: vec![b'x'] },
      Token::Plus,
      Token::Ident { bytes: vec![b'y'] },
      Token::RightBrace,
      Token::RightBrace,
      // "hello, world"
      Token::String {
        bytes: vec![
          b'h', b'e', b'l', b'l', b'o', b',', b' ', b'w', b'o', b'r', b'l', b'd',
        ],
      },
      // let lang = "monkey";
      Token::Let,
      Token::Ident {
        bytes: vec![b'l', b'a', b'n', b'g'],
      },
      Token::Assign,
      Token::String {
        bytes: vec![b'm', b'o', b'n', b'k', b'e', b'y'],
      },
      Token::Semicolon,
      // [1, 2];
      Token::LeftBracket,
      Token::Number { bytes: vec![b'1'] },
      Token::Comma,
      Token::Number { bytes: vec![b'2'] },
      Token::RightBracket,
      Token::Semicolon,
      // { "foo": "bar" };
      Token::LeftBrace,
      Token::String {
        bytes: vec![b'f', b'o', b'o'],
      },
      Token::Colon,
      Token::String {
        bytes: vec![b'b', b'a', b'r'],
      },
      Token::RightBrace,
      Token::Semicolon,
    ];

    tokens
      .into_iter()
      .zip(expected)
      .for_each(|(token, expected)| {
        assert_eq!(token, expected);
      });
  }
}
