use std::fmt;

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

type BuiltInResult = Result<Object, BuiltInError>;

/// Represents a function built into Monkey, similar to a standard lib.
#[derive(Clone, Debug)]
pub struct BuiltInFunction {
  pub function: fn(Vec<Object>) -> BuiltInResult,
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
fn len(mut args: Vec<Object>) -> BuiltInResult {
  if args.len() > 1 {
    return Err(BuiltInError::InvalidArgumentCount {
      built_in_name: "len",
      expected_count: 1,
      received_count: args.len(),
    });
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
      Err(BuiltInError::InvalidArgumentType {
        built_in_name: "len",
        argument: other_obj,
      })
    },
    None => {
      Err(BuiltInError::InvalidArgumentCount {
        built_in_name: "len",
        expected_count: 1,
        received_count: 0,
      })
    },
  }
}

/// Provides functionality for getting a clone of the first item in a Monkey
/// array (if any).
fn first(mut args: Vec<Object>) -> BuiltInResult {
  if args.len() > 1 {
    return Err(BuiltInError::InvalidArgumentCount {
      built_in_name: "first",
      expected_count: 1,
      received_count: args.len(),
    });
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      match monkey_arr.items.first() {
        Some(obj) => Ok(obj.clone()),
        _ => Ok(Object::Null(MonkeyNull)),
      }
    },
    Some(other_obj) => {
      Err(BuiltInError::InvalidArgumentType {
        built_in_name: "first",
        argument: other_obj,
      })
    },
    None => {
      Err(BuiltInError::InvalidArgumentCount {
        built_in_name: "first",
        expected_count: 1,
        received_count: 0,
      })
    },
  }
}

/// Provides functionality for getting a clone of the last item in a Monkey
/// array (if any).
fn last(mut args: Vec<Object>) -> BuiltInResult {
  if args.len() > 1 {
    return Err(BuiltInError::InvalidArgumentCount {
      built_in_name: "last",
      expected_count: 1,
      received_count: args.len(),
    });
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      match monkey_arr.items.last() {
        Some(obj) => Ok(obj.clone()),
        _ => Ok(Object::Null(MonkeyNull)),
      }
    },
    Some(other_obj) => {
      Err(BuiltInError::InvalidArgumentType {
        built_in_name: "last",
        argument: other_obj,
      })
    },
    None => {
      Err(BuiltInError::InvalidArgumentCount {
        built_in_name: "last",
        expected_count: 1,
        received_count: 0,
      })
    },
  }
}

/// Provides functionality for getting a new Monkey array containing a clone of
/// every item from the given array, except for the first item.
fn rest(mut args: Vec<Object>) -> BuiltInResult {
  if args.len() > 1 {
    return Err(BuiltInError::InvalidArgumentCount {
      built_in_name: "rest",
      expected_count: 1,
      received_count: args.len(),
    });
  }

  match args.pop() {
    Some(Object::Array(monkey_arr)) => {
      Ok(Object::Array(MonkeyArray {
        items: monkey_arr.items.into_iter().skip(1).collect(),
      }))
    },
    Some(other_obj) => {
      Err(BuiltInError::InvalidArgumentType {
        built_in_name: "rest",
        argument: other_obj,
      })
    },
    None => {
      Err(BuiltInError::InvalidArgumentCount {
        built_in_name: "rest",
        expected_count: 1,
        received_count: 0,
      })
    },
  }
}

/// Provides functionality for getting a new Monkey array containing a clone
/// of every item from the given array plus the given item appened.
fn push(args: Vec<Object>) -> BuiltInResult {
  if args.len() > 2 {
    return Err(BuiltInError::InvalidArgumentCount {
      built_in_name: "push",
      expected_count: 2,
      received_count: args.len(),
    });
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
          Err(BuiltInError::InvalidArgumentCount {
            built_in_name: "push",
            expected_count: 2,
            received_count: 1,
          })
        },
      }
    },
    Some(other_obj) => {
      Err(BuiltInError::InvalidArgumentType {
        built_in_name: "push",
        argument: other_obj.clone(),
      })
    },
    None => {
      Err(BuiltInError::InvalidArgumentCount {
        built_in_name: "push",
        expected_count: 2,
        received_count: 0,
      })
    },
  }
}

// -- Errors ------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum BuiltInError {
  InvalidArgumentCount {
    built_in_name: &'static str,
    expected_count: usize,
    received_count: usize,
  },
  InvalidArgumentType {
    built_in_name: &'static str,
    argument: Object,
  },
}

impl fmt::Display for BuiltInError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BuiltInError::InvalidArgumentCount {
        built_in_name,
        expected_count,
        received_count,
      } => {
        write!(
          f,
          "expected {} to be called with {} argument(s), but received {} instead",
          built_in_name, expected_count, received_count,
        )
      },
      BuiltInError::InvalidArgumentType {
        built_in_name,
        argument,
      } => {
        write!(
          f,
          "unexpected {} argument passed to {}",
          argument, built_in_name,
        )
      },
    }
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
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "len",
          expected_count: 1,
          received_count: 2,
        }),
      ),
      (
        vec![],
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "len",
          expected_count: 1,
          received_count: 0,
        }),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(BuiltInError::InvalidArgumentType {
          built_in_name: "len",
          argument: Object::Null(MonkeyNull),
        }),
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
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "first",
          expected_count: 1,
          received_count: 2,
        }),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(BuiltInError::InvalidArgumentType {
          built_in_name: "first",
          argument: Object::Null(MonkeyNull),
        }),
      ),
      (
        vec![],
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "first",
          expected_count: 1,
          received_count: 0,
        }),
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
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "last",
          expected_count: 1,
          received_count: 2,
        }),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(BuiltInError::InvalidArgumentType {
          built_in_name: "last",
          argument: Object::Null(MonkeyNull),
        }),
      ),
      (
        vec![],
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "last",
          expected_count: 1,
          received_count: 0,
        }),
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
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "rest",
          expected_count: 1,
          received_count: 2,
        }),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(BuiltInError::InvalidArgumentType {
          built_in_name: "rest",
          argument: Object::Null(MonkeyNull),
        }),
      ),
      (
        vec![],
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "rest",
          expected_count: 1,
          received_count: 0,
        }),
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
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "push",
          expected_count: 2,
          received_count: 3,
        }),
      ),
      (
        vec![Object::Array(MonkeyArray { items: vec![] })],
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "push",
          expected_count: 2,
          received_count: 1,
        }),
      ),
      (
        vec![Object::Null(MonkeyNull)],
        Err(BuiltInError::InvalidArgumentType {
          built_in_name: "push",
          argument: Object::Null(MonkeyNull),
        }),
      ),
      (
        vec![],
        Err(BuiltInError::InvalidArgumentCount {
          built_in_name: "push",
          expected_count: 2,
          received_count: 0,
        }),
      ),
    ];

    for (input, expected) in input_and_expected_output {
      assert_eq!(push(input), expected);
    }
  }
}
