use crate::parser::{Expression, Statement};
use crate::eval::{Scope, Value};


pub trait Eval<'a> {
    fn eval(&self, scope: &mut Scope<'a>) -> Value;
}

impl<'a> Eval<'a> for Expression {
    fn eval(&self, scope: &mut Scope<'a>) -> Value {
        use Expression::*;
        match self {
            Number(n) => Value::Number(*n),
            String(s) => Value::String(s.to_string()),
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

pub trait Run<'a> {
    fn run(&self, scope: &mut Scope<'a>);
}

impl<'a> Run<'a> for Statement {
    fn run(&self, scope: &mut Scope<'a>) {
        use Statement::*;
        match self {
            Print(e) => println!("{}", e.eval(scope)),

            // ignore result
            Expression(e) => { e.eval(scope); },
        }
    }
}