#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
}

impl From<Value> for f64 {
    fn from(val: Value) -> f64 {
        use Value::*;
        match val {
            Number(n) => n,
            _ => panic!("can't convert {:?} to number", val),
        }
    }
}

impl From<Value> for bool {
    fn from(val: Value) -> bool {
        use Value::*;
        match val {
            Boolean(n) => n,
            Nil => false,
            _ => true,
        }
    }
}

impl From<Value> for String {
    fn from(val: Value) -> String {
        use Value::*;
        match val {
            String(s) => s,
            _ => panic!("can't convert {:?} to string", val),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Value::*;
        match self {
            Number(n) => write!(fmt, "{}", n),
            Boolean(b) => write!(fmt, "{}", b),
            String(s) => write!(fmt, "{}", s),
            Nil => write!(fmt, "nil"),
        }
    }
}

macro_rules! impl_binop {
    ($tr:path, $name:ident, $op:tt) => {
        impl $tr for Value {
            type Output = Value;
            fn $name(self, rhs: Value) -> Self::Output {
                use Value::Number;
                match (self, rhs) {
                    (Number(a), Number(b)) => Number(a $op b),
                    _ => panic!("tried to do {} on something which is not a number", stringify!($op)),
                }
            }
        }
    }
}

impl_binop!(std::ops::Sub<Value>, sub, -);
impl_binop!(std::ops::Div<Value>, div, /);
impl_binop!(std::ops::Mul<Value>, mul, *);

impl std::ops::Add<Value> for Value {
    type Output = Value;
    fn add(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (String(ref a), String(ref b)) => String("".to_string() + a + b),
            _ => panic!("tried to do + on pair which aren't num+num or str+str"),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Value;
    fn neg(self) -> Self::Output {
        use Value::Number;
        match self {
            Number(n) => Number(-n),
            _ => panic!("tried to do - on non-num"),
        }
    }
}

impl std::ops::Not for Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        let b: bool = self.into();
        Value::Boolean(!b)
    }
}

impl std::cmp::PartialEq<Value> for Value {
    fn eq(&self, rhs: &Value) -> bool {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a == b,
            (Boolean(a), Boolean(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Nil, Nil) => true,

            (Number(_) | Boolean(_) | String(_) | Nil, _) => {
                panic!("tried to compare inequal types")
            }
        }
    }
}

impl std::cmp::PartialOrd<Value> for Value {
    fn partial_cmp(&self, rhs: &Value) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.partial_cmp(b),
            _ => panic!("tried to do a numerical comparison on nun-nums"),
        }
    }
}
