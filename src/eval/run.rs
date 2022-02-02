use crate::eval::{Scope, Value};
use crate::parser::{Declaration, Expression, Statement};

pub trait Eval {
    fn eval(&self, scope: &mut Scope) -> Value;
}

impl Eval for Expression {
    fn eval(&self, scope: &mut Scope) -> Value {
        use Expression::*;
        match self {
            Number(n) => Value::Number(*n),
            String(s) => Value::String(s.to_string()),
            Identifier(i) => scope.get(i),
            True => Value::Boolean(true),
            False => Value::Boolean(false),
            Nil => Value::Nil,

            Negative(a) => -a.eval(scope),
            Negate(a) => !a.eval(scope),

            Add(a, b) => a.eval(scope) + b.eval(scope),
            Subtract(a, b) => a.eval(scope) - b.eval(scope),
            Multiply(a, b) => a.eval(scope) * b.eval(scope),
            Divide(a, b) => a.eval(scope) / b.eval(scope),

            Equal(a, b) => Value::Boolean(a.eval(scope) == b.eval(scope)),
            NotEqual(a, b) => Value::Boolean(a.eval(scope) != b.eval(scope)),
            Less(a, b) => Value::Boolean(a.eval(scope) < b.eval(scope)),
            LessEqual(a, b) => Value::Boolean(a.eval(scope) <= b.eval(scope)),
            Greater(a, b) => Value::Boolean(a.eval(scope) > b.eval(scope)),
            GreaterEqual(a, b) => Value::Boolean(a.eval(scope) >= b.eval(scope)),
        }
    }
}

pub trait Run {
    fn run(&self, scope: &mut Scope);
}

impl Run for Statement {
    fn run(&self, scope: &mut Scope) {
        use Statement::*;
        match self {
            Print(e) => println!("{}", e.eval(scope)),

            Expression(e) => {
                let v = e.eval(scope);

                if scope.get("__verbose").into() {
                    println!("#> {}", v);
                }
            }

            Assignment(i, e) => {
                // assert!(scope.has(i));

                let v = e.eval(scope);

                if scope.get("__verbose").into() {
                    println!("#= {}", v);
                }

                scope.set(i, v);
            }
        }
    }
}

impl Run for Declaration {
    fn run(&self, scope: &mut Scope) {
        use Declaration::*;
        match self {
            Variable(i, e) => {
                assert!(!scope.has(i));

                let v = e.eval(scope);

                if scope.get("__verbose").into() {
                    println!("#= {}", v);
                }

                scope.set(i, v);
            }

            Statement(s) => s.run(scope),
        }
    }
}
