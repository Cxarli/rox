use crate::parser::{Expression, Statement};
use std::collections::HashMap;

pub trait TScope<'a> {
    fn get(&self, key: &str) -> Value<'a>;
}

pub struct Scope<'a> {
    pub vars: HashMap<&'a str, Value<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope { vars: HashMap::new() }
    }
}

impl<'a> TScope<'a> for Scope<'a> {
    fn get(&self, _key: &str) -> Value<'a> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Value<'a> {
    Number(f64),
    Boolean(bool),
    String(&'a str),
    Nil,
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Value::*;
        match self {
            Number(n) => write!(fmt, "{}", n),
            Boolean(b) => write!(fmt, "{}", b),
            String(s) => write!(fmt, "\"{}\"", s),
            Nil => write!(fmt, "nil"),
        }
    }
}

macro_rules! impl_binop {
    ($tr:path, $name:ident, $op:tt) => {
        impl<'a> $tr for &Value<'a> {
            type Output = Value<'a>;
            fn $name(self, rhs: &Value) -> Self::Output {
                use Value::Number;
                match (self, rhs) {
                    (Number(a), Number(b)) => Number(a $op b),
                    _ => panic!("tried to do {} on something which is not a number", stringify!($op)),
                }
            }
        }
    }
}

impl_binop!(std::ops::Add<&Value<'a>>, add, +);  // TODO: str concat
impl_binop!(std::ops::Sub<&Value<'a>>, sub, -);
impl_binop!(std::ops::Div<&Value<'a>>, div, /);
impl_binop!(std::ops::Mul<&Value<'a>>, mul, *);

impl<'a> std::ops::Neg for &Value<'a> {
    type Output = Value<'a>;
    fn neg(self) -> Self::Output {
        use Value::Number;
        match self {
            Number(n) => Number(-n),
            _ => panic!("tried to do - on non-num"),
        }
    }
}

impl<'a> std::ops::Not for &Value<'a> {
    type Output = Value<'a>;
    fn not(self) -> Self::Output {
        use Value::Boolean;
        match self {
            Boolean(b) => Boolean(!b),
            _ => panic!("tried to do ! on non-bool"),
        }
    }
}

impl<'a> std::cmp::PartialEq<Value<'a>> for Value<'a> {
    fn eq(&self, rhs: &Value) -> bool {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a == b,
            (Boolean(a), Boolean(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Nil, Nil) => true,

            (Number(_) | Boolean(_) | String(_) | Nil, _) => panic!("tried to compare inequal types"),
        }
    }
}

impl<'a> std::cmp::PartialOrd<Value<'a>> for Value<'a> {
    fn partial_cmp(&self, rhs: &Value) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.partial_cmp(b),
            _ => panic!("tried to do a numerical comparison on nun-nums"),
        }
    }
}



pub trait Eval<'a> {
    fn eval(&self, scope: &mut Scope<'a>) -> Value<'a>;
}

pub trait Run<'a> {
    fn run(&self, scope: &mut Scope<'a>);
}

impl<'a> Eval<'a> for Expression<'a> {
    fn eval(&self, scope: &mut Scope<'a>) -> Value<'a> {
        use Expression::*;
        match self {
            Number(n) => Value::Number(*n),
            String(s) => Value::String(*s),
            True => Value::Boolean(true),
            False => Value::Boolean(false),
            Nil => Value::Nil,

            Identifier(i) => scope.get(i),

            Negative(a) => - &a.eval(scope),
            Negate(a) => ! &a.eval(scope),

            Add(a, b) => &a.eval(scope) + &b.eval(scope),
            Subtract(a, b) => &a.eval(scope) - &b.eval(scope),
            Multiply(a, b) => &a.eval(scope) * &b.eval(scope),
            Divide(a, b) => &a.eval(scope) / &b.eval(scope),

            Equal(a, b) => Value::Boolean(a.eval(scope) == b.eval(scope)),
            NotEqual(a, b) => Value::Boolean(a.eval(scope) != b.eval(scope)),
            Less(a, b) => Value::Boolean(a.eval(scope) < b.eval(scope)),
            LessEqual(a, b) => Value::Boolean(a.eval(scope) <= b.eval(scope)),
            Greater(a, b) => Value::Boolean(a.eval(scope) > b.eval(scope)),
            GreaterEqual(a, b) => Value::Boolean(a.eval(scope) >= b.eval(scope)),
        }
    }
}

impl<'a> Run<'a> for Statement<'a> {
    fn run(&self, scope: &mut Scope<'a>) {
        use Statement::*;
        match self {
            Print(e) => println!("{}", e.eval(scope)),
        }
    }
}