use std::collections::HashMap;
use crate::eval::Value;

pub struct Scope<'a> {
    pub vars: HashMap<&'a str, Value>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope { vars: HashMap::new() }
    }

    pub fn get(&self, _key: &str) -> Value {
        unimplemented!()
    }
}