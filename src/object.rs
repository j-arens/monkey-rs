use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::ast::{BlockStatement, IdentExpression};
use crate::built_in::BuiltInFunction;
use crate::scope;

/// Structure that represents the values generated from evaluation.
#[derive(Clone, Debug)]
pub enum Object {
  Array(MonkeyArray),
  Bool(MonkeyBool),
  BuiltIn(MonkeyBuiltInFunction),
  Function(MonkeyFunction),
  Hash(MonkeyHash),
  Integer(MonkeyInteger),
  Null(MonkeyNull),
  Return(MonkeyReturn),
  String(MonkeyString),
}

impl Object {
  pub fn index(&self, index: Object) -> Result<Self, ObjectError> {
    match self {
      Object::Array(MonkeyArray { items }) => {
        match index {
          Object::Integer(monkey_int) => {
            match items.get(monkey_int.value as usize) {
              Some(obj) => Ok(obj.clone()),
              None => Ok(Object::Null(MonkeyNull)),
            }
          },
          other_obj => {
            Err(ObjectError::InvalidArrayIndex(other_obj))
          },
        }
      },
      Object::Hash(MonkeyHash { map }) => {
        let hash_key = MonkeyHashKey::try_from(index)?;

        match map.get(&hash_key) {
          Some(obj) => Ok(obj.clone()),
          None => Ok(Object::Null(MonkeyNull)),
        }
      },
      other_obj => {
        Err(ObjectError::ObjectNotIndexable(other_obj.clone()))
      },
    }
  }

  pub fn is_truthy(&self) -> bool {
    match self {
      Object::Bool(monkey_bool) => monkey_bool.value,
      Object::Integer(monkey_int) => monkey_int.value > 0,
      Object::String(monkey_str) => !monkey_str.value.is_empty(),
      _ => false,
    }
  }
}

// Implements + operator overload for Monkey objects in Rust.
impl Add for Object {
  type Output = Result<Self, ObjectError>;

  fn add(self, other: Self) -> Self::Output {
    match (&self, &other) {
      (Object::Integer(int_a), Object::Integer(int_b)) => {
        Ok(Object::Integer(MonkeyInteger {
          value: int_a.value + int_b.value,
        }))
      },
      (Object::String(str_a), Object::String(str_b)) => {
        let mut new_string = str_a.value.to_owned();

        new_string.push_str(&str_b.value);

        Ok(Object::String(MonkeyString { value: new_string }))
      },
      _ => Err(ObjectError::ObjectsNotAddable(self, other)),
    }
  }
}

// Implements / operator overload for Monkey objects in Rust.
impl Div for Object {
  type Output = Result<Self, ObjectError>;

  fn div(self, other: Self) -> Self::Output {
    match (&self, &other) {
      (Object::Integer(int_a), Object::Integer(int_b)) => {
        Ok(Object::Integer(MonkeyInteger {
          value: int_a.value / int_b.value,
        }))
      },
      _ => Err(ObjectError::ObjectsNotDivisable(self, other)),
    }
  }
}

// Implements * operator overload for Monkey objects in Rust.
impl Mul for Object {
  type Output = Result<Self, ObjectError>;

  fn mul(self, other: Self) -> Self::Output {
    match (&self, &other) {
      (Object::Integer(int_a), Object::Integer(int_b)) => {
        Ok(Object::Integer(MonkeyInteger {
          value: int_a.value * int_b.value,
        }))
      },
      _ => Err(ObjectError::ObjectsNotMultipliable(self, other)),
    }
  }
}

// Implements - (negate) operator overload for Monkey objects in Rust.
impl Neg for Object {
  type Output = Result<Self, ObjectError>;

  fn neg(self) -> Self::Output {
    match self {
      Object::Integer(monkey_int) => {
        Ok(Object::Integer(MonkeyInteger {
          value: -monkey_int.value,
        }))
      },
      other_obj => {
        Err(ObjectError::ObjectNotNegateable(other_obj))
      },
    }
  }
}

// Implements - (subtract) operator overload for Monkey objects in Rust.
impl Sub for Object {
  type Output = Result<Self, ObjectError>;

  fn sub(self, other: Self) -> Self::Output {
    match (&self, &other) {
      (Object::Integer(int_a), Object::Integer(int_b)) => {
        Ok(Object::Integer(MonkeyInteger {
          value: int_a.value - int_b.value,
        }))
      },
      _ => Err(ObjectError::ObjectsNotSubtractable(self, other)),
    }
  }
}

// Implements equality comparison and == or != operator overload for Monkey
// objects in Rust.
impl PartialEq for Object {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      #[cfg(test)] // Comparing arrays is enabled for internal testing only.
      (Object::Array(arr_a), Object::Array(arr_b)) => arr_a == arr_b,
      (Object::Bool(bool_a), Object::Bool(bool_b)) => bool_a.value == bool_b.value,
      (Object::Integer(int_a), Object::Integer(int_b)) => int_a.value == int_b.value,
      (Object::String(str_a), Object::String(str_b)) => str_a.value == str_b.value,
      (Object::Null(..), Object::Null(..)) => true,
      _ => false,
    }
  }
}

// Implements <, >, <=, >= operator overloads for Monkey objects in Rust.
impl PartialOrd for Object {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match (self, other) {
      (Object::Integer(int_a), Object::Integer(int_b)) => {
        if int_a.value > int_b.value {
          return Some(Ordering::Greater);
        }

        if int_a.value < int_b.value {
          return Some(Ordering::Less);
        }

        Some(Ordering::Equal)
      },
      _ => None,
    }
  }
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Object::Array(monkey_arr) => write!(f, "{}", monkey_arr),
      Object::Bool(monkey_bool) => write!(f, "{}", monkey_bool),
      Object::BuiltIn(monkey_built_in) => write!(f, "{}", monkey_built_in),
      Object::Function(monkey_fn) => write!(f, "{}", monkey_fn),
      Object::Hash(monkey_hash) => write!(f, "{}", monkey_hash),
      Object::Integer(monkey_int) => write!(f, "{}", monkey_int),
      Object::Null(monkey_null) => write!(f, "{}", monkey_null),
      Object::Return(monkey_return) => write!(f, "{}", monkey_return),
      Object::String(monkey_str) => write!(f, "{}", monkey_str),
    }
  }
}

// -- Monkey Data Types -------------------------------------------------------

// Array

#[derive(Clone, Debug, PartialEq)]
pub struct MonkeyArray {
  pub items: Vec<Object>,
}

impl fmt::Display for MonkeyArray {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "({})[{}]",
      self.items.len(),
      self
        .items
        .iter()
        .map(|obj| format!("{}", obj))
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

// Boolean

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct MonkeyBool {
  pub value: bool,
}

impl fmt::Display for MonkeyBool {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

// Built-in

#[derive(Clone, Debug)]
pub struct MonkeyBuiltInFunction {
  pub value: BuiltInFunction,
}

impl fmt::Display for MonkeyBuiltInFunction {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[built-in function: `{}`]", self.value.name)
  }
}

// Function

#[derive(Clone, Debug)]
pub struct MonkeyFunction {
  pub body: BlockStatement,
  pub parameters: Vec<IdentExpression>,
  pub scope: scope::ShareableScope,
}

impl fmt::Display for MonkeyFunction {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[function]")
  }
}

// Hash

#[derive(Clone, Debug)]
pub struct MonkeyHash {
  pub map: HashMap<MonkeyHashKey, Object>,
}

impl fmt::Display for MonkeyHash {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut formatted_pairs: Vec<_> = self
      .map
      .iter()
      .map(|(key, value)| format!("{}: {}", key, value))
      .collect();

    // Order pairs alphabetically to produce deterministic output for testing
    // since `HashMap` insertion order is random.
    #[allow(clippy::unnecessary_sort_by)]
    formatted_pairs.sort_by(|a, b| a.to_lowercase().cmp(&b.to_lowercase()));

    write!(f, "{{{}}}", formatted_pairs.join(", "))
  }
}

// Constraining which Monkey data types can be used as hash keys prevents
// having to implement `Hash` for any `Object`, which is a can of worms.
#[derive(Clone, Debug, Eq)]
pub enum MonkeyHashKey {
  Bool(MonkeyBool),
  Integer(MonkeyInteger),
  String(MonkeyString),
}

impl Hash for MonkeyHashKey {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      MonkeyHashKey::Bool(monkey_bool) => monkey_bool.hash(state),
      MonkeyHashKey::Integer(monkey_int) => monkey_int.hash(state),
      MonkeyHashKey::String(monkey_str) => monkey_str.hash(state),
    }
  }
}

impl PartialEq for MonkeyHashKey {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (MonkeyHashKey::Bool(bool_a), MonkeyHashKey::Bool(bool_b)) => bool_a == bool_b,
      (MonkeyHashKey::Integer(int_a), MonkeyHashKey::Integer(int_b)) => int_a == int_b,
      (MonkeyHashKey::String(str_a), MonkeyHashKey::String(str_b)) => str_a == str_b,
      _ => false,
    }
  }
}

impl TryFrom<Object> for MonkeyHashKey {
  type Error = ObjectError;

  fn try_from(object: Object) -> Result<Self, Self::Error> {
    match object {
      Object::Bool(monkey_bool) => Ok(MonkeyHashKey::Bool(monkey_bool)),
      Object::Integer(monkey_int) => Ok(MonkeyHashKey::Integer(monkey_int)),
      Object::String(monkey_str) => Ok(MonkeyHashKey::String(monkey_str)),
      other_obj => Err(ObjectError::ObjectNotHashable(other_obj)),
    }
  }
}

impl fmt::Display for MonkeyHashKey {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      MonkeyHashKey::Bool(monkey_bool) => write!(f, "{}", monkey_bool),
      MonkeyHashKey::Integer(monkey_int) => write!(f, "{}", monkey_int),
      MonkeyHashKey::String(monkey_str) => write!(f, "{}", monkey_str),
    }
  }
}

// Integer

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct MonkeyInteger {
  pub value: i64,
}

impl fmt::Display for MonkeyInteger {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

// Null

#[derive(Clone, Debug)]
pub struct MonkeyNull;

impl fmt::Display for MonkeyNull {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[null]")
  }
}

// Return

#[derive(Clone, Debug)]
pub struct MonkeyReturn {
  pub value: Box<Object>,
}

impl fmt::Display for MonkeyReturn {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

// String

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct MonkeyString {
  pub value: String,
}

impl fmt::Display for MonkeyString {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

// -- Errors ------------------------------------------------------------------

#[derive(Debug)]
pub enum ObjectError {
  InvalidArrayIndex(Object),
  ObjectNotHashable(Object),
  ObjectNotIndexable(Object),
  ObjectsNotAddable(Object, Object),
  ObjectsNotDivisable(Object, Object),
  ObjectsNotMultipliable(Object, Object),
  ObjectNotNegateable(Object),
  ObjectsNotSubtractable(Object, Object),
}

impl fmt::Display for ObjectError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ObjectError::InvalidArrayIndex(obj) => {
        write!(f, "cannot index array with `{}`", obj)
      },
      ObjectError::ObjectNotHashable(obj) => {
        write!(f, "cannot use `{}` as hash key", obj)
      },
      ObjectError::ObjectNotIndexable(obj) => {
        write!(f, "cannot index `{}`", obj)
      },
      ObjectError::ObjectsNotAddable(obj_a, obj_b) => {
        write!(f, "cannot add or concatenate `{}` and `{}`", obj_a, obj_b)
      },
      ObjectError::ObjectsNotDivisable(obj_a, obj_b) => {
        write!(f, "cannot divide `{}` by `{}`", obj_a, obj_b)
      },
      ObjectError::ObjectsNotMultipliable(obj_a, obj_b) => {
        write!(f, "cannot multiply `{}` by `{}`", obj_a, obj_b)
      },
      ObjectError::ObjectNotNegateable(obj) => {
        write!(f, "cannot negate `{}`", obj)
      },
      ObjectError::ObjectsNotSubtractable(obj_a, obj_b) => {
        write!(f, "cannot subtract `{}` by `{}`", obj_a, obj_b)
      },
    }
  }
}
