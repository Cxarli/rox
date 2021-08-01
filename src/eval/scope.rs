use std::collections::HashMap;
use crate::eval::Value;

pub struct Scope {
    pub vars: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { vars: HashMap::new() }
    }

    pub fn get(&self, key: &str) -> Value {
        self.vars.get(key)
            .map(|x| x.clone())
            .unwrap_or(Value::Nil)
    }

    pub fn set(&mut self, key: &str, val: Value) {
        // silently overwrites
        self.vars.insert(key.to_string(), val);
    }

    pub fn has(&self, key: &str) -> bool {
        self.vars.contains_key(key)
    }
}