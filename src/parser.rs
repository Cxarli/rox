use super::lexer::{Token, SpannedToken};

pub type Expr<'a> = Box<Expression<'a>>;

#[derive(Debug)]
pub enum Statement<'a> {
    Print(Expression<'a>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    // Literals
    Number(f64),
    String(&'a str),
    Identifier(&'a str),
    True,
    False,
    Nil,

    // Unary
    Negative(Expr<'a>),
    Negate(Expr<'a>),

    // Arithmetic
    Add(Expr<'a>, Expr<'a>),
    Subtract(Expr<'a>, Expr<'a>),
    Multiply(Expr<'a>, Expr<'a>),
    Divide(Expr<'a>, Expr<'a>),

    // Compare
    Equal(Expr<'a>, Expr<'a>),
    NotEqual(Expr<'a>, Expr<'a>),
    Less(Expr<'a>, Expr<'a>),
    LessEqual(Expr<'a>, Expr<'a>),
    Greater(Expr<'a>, Expr<'a>),
    GreaterEqual(Expr<'a>, Expr<'a>),
}

impl<'a> std::fmt::Display for Statement<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Statement::*;
        match self {
            Print(e) => write!(fmt, "print {};", e),
        }
    }
}

impl<'a> std::fmt::Display for Expression<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Expression::*;
        match self {
            Number(n) => write!(fmt, "{}", n),
            String(s) => write!(fmt, r#""{}""#, s),
            Identifier(i) => write!(fmt, "{}", i),
            True => write!(fmt, "true"),
            False => write!(fmt, "false"),
            Nil => write!(fmt, "nil"),

            Negative(e) => write!(fmt, "-({})", e),
            Negate(e) => write!(fmt, "!({})", e),

            Add(a, b) => write!(fmt, "({}) + ({})", a, b),
            Subtract(a, b) => write!(fmt, "({}) - ({})", a, b),
            Multiply(a, b) => write!(fmt, "({}) * ({})", a, b),
            Divide(a, b) => write!(fmt, "({}) / ({})", a, b),

            Equal(a, b) => write!(fmt, "({}) == ({})", a, b),
            NotEqual(a, b) => write!(fmt, "({}) != ({})", a, b),
            Less(a, b) => write!(fmt, "({}) < ({})", a, b),
            LessEqual(a, b) => write!(fmt, "({}) <= ({})", a, b),
            Greater(a, b) => write!(fmt, "({}) > ({})", a, b),
            GreaterEqual(a, b) => write!(fmt, "({}) >= ({})", a, b),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Unmatched,
    Failure(String),
}

pub type Tokens<'a> = &'a [SpannedToken<'a>];
pub type SingleExpr<'a> = std::result::Result<Expression<'a>, Error>;
pub type SingleStmt<'a> = std::result::Result<Statement<'a>, Error>;
pub type YieldExpr<'a> = std::result::Result<(Tokens<'a>, Expression<'a>), Error>;
pub type YieldStmt<'a> = std::result::Result<(Tokens<'a>, Statement<'a>), Error>;


fn parse_num(s: &str) -> SingleExpr {
    s.parse::<f64>()
        .map(Expression::Number)
        .map_err(|e| Error::Failure(e.to_string()))
}

fn parse_str(s: &str) -> SingleExpr {
    // TODO: proper escaping
    Ok(Expression::String(&s[1..s.len()-1]))
}

pub fn main(mut tokens: Tokens) -> Result<Vec<Statement>, Vec<Error>> {
    let mut stmts = vec!{};
    let mut errs = vec!{};

    loop {
        match statement(tokens) {
            Ok((left, stmt)) => {
                stmts.push(stmt);

                if left.is_empty() {
                    break;
                }
                tokens = left;

                continue;
            },
            Err(Error::Unmatched) => {},
            Err(f @ Error::Failure(_)) => {
                errs.push(f);
                
                // do error recovery:
                // ignore all tokens up to the next semicolon,
                // then try making another statement
                let start = tokens.get(0).unwrap();
                loop {
                    match tokens.get(0) {
                        None => return Err(errs),
                        Some(end @ (_, Token::Semicolon)) => {
                            tokens = &tokens[1..];

                            if statement(tokens).is_ok() {
                                println!("skipped from {:?} to {:?}, next up is {:?}", start, end, tokens.get(0));
                                break;
                            }
                        },
                        Some(_) => {
                            // skip this
                            tokens = &tokens[1..];
                        }
                    }
                }
            },
        }
    }

    if errs.is_empty() {
        Ok(stmts)
    } else {
        Err(errs)
    }
}

fn statement(tokens: Tokens) -> YieldStmt {
    macro_rules! try_stmt {
        ($fn:expr) => {
            match $fn(tokens) {
                Ok(o @ _) => return Ok(o),
                Err(f @ Error::Failure(_)) => return Err(f),
                Err(Error::Unmatched) => {},
            }
        }
    }

    if tokens.is_empty() {
        return Err(Error::Unmatched);
    }

    try_stmt!(print);

    Err(Error::Failure(format!("no valid stmt found starting at {:?}", tokens.get(0))))
}

fn print(tokens: Tokens) -> YieldStmt {
    match tokens.get(0) {
        Some((span, Token::Print)) => {
            match expression(&tokens[1..]) {
                Ok((rest, expr)) => {
                    match rest.get(0) {
                        Some((_, Token::Semicolon)) => Ok((&rest[1..], Statement::Print(expr))),
                        r @ (None | Some(_)) => Err(Error::Failure(format!(
                            "[{:?}] expected semicolon after expression {:?} after print, but found {:?}", span, expr, r))),
                    }
                },
                Err(Error::Unmatched) => Err(Error::Failure(format!("[{:?}] expected expression after print", span))),
                Err(f @ Error::Failure(_)) => Err(f),
            }
        },
        None | Some(_) => Err(Error::Unmatched),
    }
}

macro_rules! repeat_las_binop {
    ($tokens: expr, $lower:expr, { $($map_from:path => $map_to:path),+ $(,)? }) => {
        match $lower($tokens) {
            Ok((mut outer_rest, mut outer_expr)) => {
                loop {
                    match outer_rest.get(0) {
                        Some((span, t @ ( $($map_from)|+ ))) => {
                            let ex = match t {
                                $($map_from => $map_to),+,
                                _ => unreachable!(),
                            };
    
                            match $lower(&outer_rest[1..]) {
                                Ok((inner_rest, inner_expr)) => {
                                    outer_expr = ex(Box::new(outer_expr), Box::new(inner_expr));
                                    outer_rest = inner_rest;
                                },
                                Err(Error::Unmatched) => return Err(Error::Failure(format!("[{:?}] in factor expected unary expression after {:?}", span, t))),
                                Err(f @ Error::Failure(_)) => return Err(f),
                            }
                        },
                        None | Some(_) => return Ok((outer_rest, outer_expr)),
                    }
                }
            },
            Err(f @ Error::Failure(_)) => return Err(f),
            Err(u @ Error::Unmatched) => Err(u),
        }
    }
}

fn expression(tokens: Tokens) -> YieldExpr {
    equality(tokens)
}

fn equality(tokens: Tokens) -> YieldExpr {
    // equality := comparison ( ( BangEqual | EqualEqual ) comparison )* ;

    repeat_las_binop!(tokens, comparison, { Token::BangEqual => Expression::NotEqual, Token::EqualEqual => Expression::Equal })
}

fn comparison(tokens: Tokens) -> YieldExpr {
    // comparison := term ( ( Greater | GreaterEqual | Less | LessEqual ) term )* ;
    repeat_las_binop!(tokens, term, {
        Token::Greater => Expression::Greater,
        Token::GreaterEqual => Expression::GreaterEqual,
        Token::Less => Expression::Less,
        Token::LessEqual => Expression::LessEqual,
    })
}

fn term(tokens: Tokens) -> YieldExpr {
    // term := factor ( ( Minus | Plus ) factor )* ;
    repeat_las_binop!(tokens, factor, { Token::Minus => Expression::Subtract, Token::Plus => Expression::Add })
}

fn factor(tokens: Tokens) -> YieldExpr {
    // factor := unary ( ( Slash | Star ) unary )* ;
    repeat_las_binop!(tokens, unary, { Token::Slash => Expression::Divide, Token::Star => Expression::Multiply })
}

fn unary(tokens: Tokens) -> YieldExpr {
    // unary := ( Bang | Minus ) unary | primary ;
    match tokens.get(0) {
        Some((span, r @ (Token::Bang | Token::Minus))) => {
            let ex = match r {
                Token::Bang => Expression::Negate,
                Token::Minus => Expression::Negative,
                _ => unreachable!(),
            };

            match unary(&tokens[1..]) {
                Ok((rest, expr)) => Ok((rest, ex(Box::new(expr)))),
                _ => Err(Error::Failure(format!("[{:?}] expected unary expression after {:?}", span, r))),
            }
        },
        Some(_) => primary(tokens),
        None => Err(Error::Unmatched),
    }
}

fn primary(tokens: Tokens) -> YieldExpr {
    // primary := Number | String | True | False | Nil | LeftParen expression RightParen
    match tokens.get(0) {
        Some((_, Token::Number(s)    )) => parse_num(s).map(|x| (&tokens[1..], x)),
        Some((_, Token::String(s)    )) => parse_str(s).map(|x| (&tokens[1..], x)),
        Some((_, Token::Identifier(s))) => Ok((&tokens[1..], Expression::Identifier(s))),
        Some((_, Token::True         )) => Ok((&tokens[1..], Expression::True)),
        Some((_, Token::False        )) => Ok((&tokens[1..], Expression::False)),
        Some((_, Token::Nil          )) => Ok((&tokens[1..], Expression::Nil)),

        Some((span, Token::LeftParen)) => {
            match expression(&tokens[1..]) {
                Ok((rest, expr)) => {
                    match rest.get(0) {
                        Some((_, Token::RightParen)) => Ok((&rest[1..], expr)),
                        _ => Err(Error::Failure(format!("[{:?}] found ( followed by expression {:?} but without )", span, expr)))
                    }
                },
                Err(Error::Unmatched) => Err(Error::Failure(format!("[{:?}] found ( not followed by expression", span))),
                f @ Err(Error::Failure(_)) => f,
            }
        }

        Some(_) | None => Err(Error::Unmatched),
    }
}
