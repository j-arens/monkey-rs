use crate::eval::EvalResult;
use crate::object::{MonkeyArray, MonkeyBuiltInFunction, MonkeyInteger, MonkeyNull, Object};

pub const BUILT_IN_FUNCTIONS: [BuiltInFunction; 5] = [
  BuiltInFunction {
    function: len,
    name: "len",
  },
  BuiltInFunction {
    function: first,
    name: "first",
  },
  BuiltInFunction {
    function: last,
    name: "last",
  },
  BuiltInFunction {
    function: rest,
    name: "rest",
  },
  BuiltInFunction {
    function: push,
    name: "push",
  },
];

/// Represents a function built into Monkey, similar to a standard lib.
#[derive(Clone, Debug)]
pub struct BuiltInFunction {
  pub function: fn(Vec<Object>) -> EvalResult,
  pub name: &'static str,
}

impl BuiltInFunction {
  pub fn into_binding_pair(self) -> (String, Object) {
    (
      self.name.to_owned(),
      Object::BuiltIn(MonkeyBuiltInFunction { value: self }),
    )
  }
}

/// Provides functionality for getting the number of items in a Monkey array,
/// or the number of bytes in a Monkey string (not `chars` or graphemes).
fn len(mut args: Vec<Object>) -> EvalResult {
  if args.len() > 1 {
    return Err(format!(
      "Expected `len` to be called with 1 argument, but received {} instead.",
      args.len()
    ));
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      Ok(Object::Integer(MonkeyInteger {
        value: monkey_arr.items.len() as i64,
      }))
    },
    Some(Object::String(monkey_str)) => {
      Ok(Object::Integer(MonkeyInteger {
        value: monkey_str.value.len() as i64,
      }))
    },
    Some(other_obj) => {
      Err(format!(
        "Expected argument to `len` to be an array or string, but received {} instead.",
        other_obj
      ))
    },
    None => {
      Err(String::from(
        "Expected `len` to be called with 1 argument, but received none instead.",
      ))
    },
  }
}

/// Provides functionality for getting a clone of the first item in a Monkey
/// array (if any).
fn first(mut args: Vec<Object>) -> EvalResult {
  if args.len() > 1 {
    return Err(format!(
      "Expected `first` to be called with 1 argument, but received {} instead.",
      args.len()
    ));
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      match monkey_arr.items.first() {
        Some(obj) => Ok(obj.clone()),
        _ => Ok(Object::Null(MonkeyNull)),
      }
    },
    Some(other_obj) => {
      Err(format!(
        "Expected argument to `first` to be an array, but received {} instead.",
        other_obj
      ))
    },
    None => {
      Err(String::from(
        "Expected `first` to be called with 1 argument, but received none instead.",
      ))
    },
  }
}

/// Provides functionality for getting a clone of the last item in a Monkey
/// array (if any).
fn last(mut args: Vec<Object>) -> EvalResult {
  if args.len() > 1 {
    return Err(format!(
      "Expected `last` to be called with 1 argument, but received {} instead.",
      args.len()
    ));
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      match monkey_arr.items.last() {
        Some(obj) => Ok(obj.clone()),
        _ => Ok(Object::Null(MonkeyNull)),
      }
    },
    Some(other_obj) => {
      Err(format!(
        "Expected argument to `last` to be an array, but received {} instead.",
        other_obj
      ))
    },
    None => {
      Err(String::from(
        "Expected `last` to be called with 1 argument, but received none instead.",
      ))
    },
  }
}

/// Provides functionality for getting a new Monkey array containing a clone of
/// every item from the given array, except for the first item.
fn rest(mut args: Vec<Object>) -> EvalResult {
  if args.len() > 1 {
    return Err(format!(
      "Expected `rest` to be called with 1 argument, but received {} instead.",
      args.len()
    ));
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      Ok(Object::Array(MonkeyArray {
        items: monkey_arr.items.into_iter().skip(1).collect(),
      }))
    },
    Some(other_obj) => {
      Err(format!(
        "Expected argument to `rest` to be an array, but received {} instead.",
        other_obj
      ))
    },
    None => {
      Err(String::from(
        "Expected `rest` to be called with 1 argument, but received none instead.",
      ))
    },
  }
}

/// Provides functionality for getting a new Monkey array containing a clone
/// of every item from the given array plus the given item appened.
fn push(args: Vec<Object>) -> EvalResult {
  if args.len() > 2 {
    return Err(format!(
      "Expected `push` to be called with 2 arguments, but received {} instead.",
      args.len()
    ));
  }

  match args.get(0) {
    Some(Object::Array(monkey_arr)) => {
      match args.get(1) {
        Some(obj) => {
          let mut new_items = monkey_arr.items.clone();

          new_items.push(obj.clone());

          Ok(Object::Array(MonkeyArray { items: new_items }))
        },
        None => {
          Err(String::from(
            "Expected `push` to be called with 2 arguments, but received only 1.",
          ))
        },
      }
    },
    Some(other_obj) => {
      Err(format!(
        "Expected the first argument of `push` to be an array, but received {} instead.",
        other_obj
      ))
    },
    None => {
      Err(String::from(
        "Expected the first argument of `push` to be an array, but received none instead.",
      ))
    },
  }
}

// -- Tests -------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use crate::object::MonkeyString;

  #[test]
  fn test_len() {
    let input_and_expected_output = vec![
      (
        vec![Object::Array(MonkeyArray { items: vec![] })],
        Ok(Object::Integer(MonkeyInteger { value: 0 })),
      ),
      (
        vec![Object::Array(MonkeyArray {
          items: vec![Object::Null(MonkeyNull)],
        })],
        Ok(Object::Integer(MonkeyInteger { value: 1 })),
      ),
      (
        vec![Object::String(MonkeyString {
          value: String::from(""),
        })],
        Ok(Object::Integer(MonkeyInteger { value: 0 })),
      ),
      (
        vec![Object::String(MonkeyString {
          value: String::from("foo"),
        })],
        Ok(Object::Integer(MonkeyInteger { value: 3 })),
      ),
      (
        vec![
          Object::Array(MonkeyArray { items: vec![] }),
          Object::Array(MonkeyArray { items: vec![] }),
        ],
        Err(String::from(
          "Expected `len` to be called with 1 argument, but received 2 instead.",
        )),
      ),
      (
        vec![],
        Err(String::from(
          "Expected `len` to be called with 1 argument, but received none instead.",
        )),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(String::from(
          "Expected argument to `len` to be an array or string, but received [null] instead.",
        )),
      ),
    ];

    for (input, expected) in input_and_expected_output {
      assert_eq!(len(input), expected);
    }
  }

  #[test]
  fn test_first() {
    let input_and_expected_output = vec![
      (
        vec![Object::Array(MonkeyArray { items: vec![] })],
        Ok(Object::Null(MonkeyNull)),
      ),
      (
        vec![Object::Array(MonkeyArray {
          items: vec![
            Object::Integer(MonkeyInteger { value: 1 }),
            Object::Integer(MonkeyInteger { value: 2 }),
          ],
        })],
        Ok(Object::Integer(MonkeyInteger { value: 1 })),
      ),
      (
        vec![
          Object::Array(MonkeyArray { items: vec![] }),
          Object::Array(MonkeyArray { items: vec![] }),
        ],
        Err(String::from(
          "Expected `first` to be called with 1 argument, but received 2 instead.",
        )),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(String::from(
          "Expected argument to `first` to be an array, but received [null] instead.",
        )),
      ),
      (
        vec![],
        Err(String::from(
          "Expected `first` to be called with 1 argument, but received none instead.",
        )),
      ),
    ];

    for (input, expected) in input_and_expected_output {
      assert_eq!(first(input), expected);
    }
  }

  #[test]
  fn test_last() {
    let input_and_expected_output = vec![
      (
        vec![Object::Array(MonkeyArray { items: vec![] })],
        Ok(Object::Null(MonkeyNull)),
      ),
      (
        vec![Object::Array(MonkeyArray {
          items: vec![
            Object::Integer(MonkeyInteger { value: 1 }),
            Object::Integer(MonkeyInteger { value: 2 }),
          ],
        })],
        Ok(Object::Integer(MonkeyInteger { value: 2 })),
      ),
      (
        vec![
          Object::Array(MonkeyArray { items: vec![] }),
          Object::Array(MonkeyArray { items: vec![] }),
        ],
        Err(String::from(
          "Expected `last` to be called with 1 argument, but received 2 instead.",
        )),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(String::from(
          "Expected argument to `last` to be an array, but received [null] instead.",
        )),
      ),
      (
        vec![],
        Err(String::from(
          "Expected `last` to be called with 1 argument, but received none instead.",
        )),
      ),
    ];

    for (input, expected) in input_and_expected_output {
      assert_eq!(last(input), expected);
    }
  }

  #[test]
  fn test_rest() {
    let input_and_expected_output = vec![
      (
        vec![Object::Array(MonkeyArray { items: vec![] })],
        Ok(Object::Array(MonkeyArray { items: vec![] })),
      ),
      (
        vec![Object::Array(MonkeyArray {
          items: vec![Object::Integer(MonkeyInteger { value: 1 })],
        })],
        Ok(Object::Array(MonkeyArray { items: vec![] })),
      ),
      (
        vec![Object::Array(MonkeyArray {
          items: vec![Object::Null(MonkeyNull), Object::Null(MonkeyNull)],
        })],
        Ok(Object::Array(MonkeyArray {
          items: vec![Object::Null(MonkeyNull)],
        })),
      ),
      (
        vec![
          Object::Array(MonkeyArray { items: vec![] }),
          Object::Array(MonkeyArray { items: vec![] }),
        ],
        Err(String::from(
          "Expected `rest` to be called with 1 argument, but received 2 instead.",
        )),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(String::from(
          "Expected argument to `rest` to be an array, but received [null] instead.",
        )),
      ),
      (
        vec![],
        Err(String::from(
          "Expected `rest` to be called with 1 argument, but received none instead.",
        )),
      ),
    ];

    for (input, expected) in input_and_expected_output {
      assert_eq!(rest(input), expected);
    }
  }

  #[test]
  fn test_push() {
    let input_and_expected_output = vec![
      (
        vec![
          Object::Array(MonkeyArray { items: vec![] }),
          Object::Null(MonkeyNull),
        ],
        Ok(Object::Array(MonkeyArray {
          items: vec![Object::Null(MonkeyNull)],
        })),
      ),
      (
        vec![
          Object::Array(MonkeyArray {
            items: vec![Object::Null(MonkeyNull)],
          }),
          Object::Null(MonkeyNull),
        ],
        Ok(Object::Array(MonkeyArray {
          items: vec![Object::Null(MonkeyNull), Object::Null(MonkeyNull)],
        })),
      ),
      (
        vec![
          Object::Array(MonkeyArray { items: vec![] }),
          Object::Null(MonkeyNull),
          Object::Null(MonkeyNull),
        ],
        Err(String::from(
          "Expected `push` to be called with 2 arguments, but received 3 instead.",
        )),
      ),
      (
        vec![Object::Array(MonkeyArray { items: vec![] })],
        Err(String::from(
          "Expected `push` to be called with 2 arguments, but received only 1.",
        )),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(String::from(
          "Expected the first argument of `push` to be an array, but received [null] instead.",
        )),
      ),
      (
        vec![],
        Err(String::from(
          "Expected the first argument of `push` to be an array, but received none instead.",
        )),
      ),
    ];

    for (input, expected) in input_and_expected_output {
      assert_eq!(push(input), expected);
    }
  }
}
