use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::built_in::BuiltInFunction;
use crate::object::Object;

// This won't detect or prevent cyclic references, and could cause memory
// leaks.
pub type ShareableScope = Rc<RefCell<Scope>>;

/// Provides a mechanism for storing and retrieving lexical bindings while
/// evaluating a Monkey program.
#[derive(Debug)]
pub struct Scope {
  bindings: HashMap<String, Object>,
  outer_scope: Option<ShareableScope>,
}

impl Scope {
  /// Creates a new nested scope.
  pub fn nested(outer_scope: ShareableScope) -> ShareableScope {
    let scope = Self {
      bindings: HashMap::new(),
      outer_scope: Some(outer_scope),
    };

    Rc::new(RefCell::new(scope))
  }

  /// Creates a new "global" scope with the given built-in functions set. A
  /// global scope should only be created once for each program, and should be
  /// the outer-most scope.
  pub fn global(built_ins: Vec<BuiltInFunction>) -> ShareableScope {
    let bindings = built_ins
      .into_iter()
      .map(|built_in| built_in.into_binding_pair())
      .collect();

    let scope = Self {
      bindings,
      outer_scope: None,
    };

    Rc::new(RefCell::new(scope))
  }

  pub fn set_binding(&mut self, key: String, object: Object) {
    self.bindings.insert(key, object);
  }

  pub fn get_binding(&self, key: &str) -> Option<Object> {
    match self.bindings.get(key) {
      Some(object) => Some(object.clone()),
      _ => self.get_outer_binding(key),
    }
  }

  fn get_outer_binding(&self, key: &str) -> Option<Object> {
    if let Some(outer_scope) = &self.outer_scope {
      return outer_scope.borrow().get_binding(key);
    }

    None
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::object::MonkeyString;

  #[test]
  fn test_getting_and_setting_bindings() {
    let global_bindings: HashMap<String, Object> = vec![(
      String::from("foo"),
      Object::String(MonkeyString {
        value: String::from("foo"),
      }),
    )]
    .into_iter()
    .collect();

    let inner_bindings: HashMap<String, Object> = vec![(
      String::from("bar"),
      Object::String(MonkeyString {
        value: String::from("bar"),
      }),
    )]
    .into_iter()
    .collect();

    let mut scope = Scope {
      bindings: inner_bindings,
      outer_scope: Some(Rc::new(RefCell::new(Scope {
        bindings: global_bindings,
        outer_scope: None,
      }))),
    };

    scope.set_binding(
      String::from("baz"),
      Object::String(MonkeyString {
        value: String::from("baz"),
      }),
    );

    assert_eq!(
      scope.get_binding("baz"),
      Some(Object::String(MonkeyString {
        value: String::from("baz")
      }))
    );

    assert_eq!(
      scope.get_binding("bar"),
      Some(Object::String(MonkeyString {
        value: String::from("bar")
      }))
    );

    assert_eq!(
      scope.get_binding("foo"),
      Some(Object::String(MonkeyString {
        value: String::from("foo")
      }))
    );

    assert_eq!(scope.get_binding("not_set"), None);
  }
}
