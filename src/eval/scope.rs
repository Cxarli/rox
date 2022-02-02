use crate::eval::Value;
use std::collections::HashMap;

#[derive(Default)]
pub struct Scope {
    pub vars: HashMap<String, Value>,
}

impl Scope {
    pub fn get(&self, key: &str) -> Value {
        self.vars.get(key).cloned().unwrap_or(Value::Nil)
    }

    pub fn set(&mut self, key: &str, val: Value) {
        // silently overwrites
        self.vars.insert(key.to_string(), val);
    }

    pub fn has(&self, key: &str) -> bool {
        self.vars.contains_key(key)
    }
}
