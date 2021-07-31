#[derive(Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
}

impl<'a> std::fmt::Display for Value {
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
        impl<'a> $tr for &Value {
            type Output = Value;
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

impl_binop!(std::ops::Sub<&Value>, sub, -);
impl_binop!(std::ops::Div<&Value>, div, /);
impl_binop!(std::ops::Mul<&Value>, mul, *);

impl<'a> std::ops::Add<&Value> for &Value {
    type Output = Value;
    fn add(self, rhs: &Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (String(ref a), String(ref b)) => String("".to_string() + a + b),
            _ => panic!("tried to do + on pair which aren't num+num or str+str"),
        }
    }
}

impl<'a> std::ops::Neg for &Value {
    type Output = Value;
    fn neg(self) -> Self::Output {
        use Value::Number;
        match self {
            Number(n) => Number(-n),
            _ => panic!("tried to do - on non-num"),
        }
    }
}

impl<'a> std::ops::Not for &Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        use Value::*;
        match self {
            // false and nil are falsey, everything else is truthy
            Nil | Boolean(false) => Boolean(true),
            _ => Boolean(false),
        }
    }
}

impl<'a> std::cmp::PartialEq<Value> for Value {
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

impl<'a> std::cmp::PartialOrd<Value> for Value {
    fn partial_cmp(&self, rhs: &Value) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.partial_cmp(b),
            _ => panic!("tried to do a numerical comparison on nun-nums"),
        }
    }
}