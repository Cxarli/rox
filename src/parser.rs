use super::lexer::{Token, SpannedToken};

pub type Expr = Box<Expression>;

#[derive(Debug)]
pub enum Statement {
    Print(Expression),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    // Literals
    Number(f64),
    String(String),
    Identifier(String),
    True,
    False,
    Nil,

    // Unary
    Negative(Expr),
    Negate(Expr),

    // Arithmetic
    Add(Expr, Expr),
    Subtract(Expr, Expr),
    Multiply(Expr, Expr),
    Divide(Expr, Expr),

    // Compare
    Equal(Expr, Expr),
    NotEqual(Expr, Expr),
    Less(Expr, Expr),
    LessEqual(Expr, Expr),
    Greater(Expr, Expr),
    GreaterEqual(Expr, Expr),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Statement::*;
        match self {
            Print(e) => write!(fmt, "print {};", e),
            Expression(e) => write!(fmt, "{};", e),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Expression::*;
        match self {
            Number(n) => write!(fmt, "{}", n),
            String(s) => write!(fmt, r#""{}""#, s),
            Identifier(i) => write!(fmt, "{}", i),
            True => write!(fmt, "true"),
            False => write!(fmt, "false"),
            Nil => write!(fmt, "nil"),

            Negative(e) => write!(fmt, "-{}", e),
            Negate(e) => write!(fmt, "!{}", e),

            Add(a, b) => write!(fmt, "({} + {})", a, b),
            Subtract(a, b) => write!(fmt, "({} - {})", a, b),
            Multiply(a, b) => write!(fmt, "({} * {})", a, b),
            Divide(a, b) => write!(fmt, "({} / {})", a, b),

            Equal(a, b) => write!(fmt, "({} == {})", a, b),
            NotEqual(a, b) => write!(fmt, "({} != {})", a, b),
            Less(a, b) => write!(fmt, "({} < {})", a, b),
            LessEqual(a, b) => write!(fmt, "({} <= {})", a, b),
            Greater(a, b) => write!(fmt, "({} > {})", a, b),
            GreaterEqual(a, b) => write!(fmt, "({} >= {})", a, b),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Unmatched,
    Failure(String),
}

#[derive(Clone, Debug)]
pub struct TokenWalker<'a> {
    pub tokens: &'a [SpannedToken],
    index: usize,
}

impl<'a> TokenWalker<'a> {
    pub fn new(tokens: &[SpannedToken]) -> TokenWalker {
        TokenWalker {
            tokens: tokens,
            index: 0,
        }
    }

    pub fn peek(&self) -> Option<&SpannedToken> {
        self.tokens.get(self.index)
    }

    pub fn is_empty(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn back(&self) -> Option<TokenWalker<'a>> {
        if self.index == 0 {
            None
        } else {
            Some(TokenWalker {
                tokens: self.tokens,
                index: self.index - 1,
            })
        }
    }
}

impl<'a> Iterator for TokenWalker<'a> {
    type Item = &'a SpannedToken;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.get(self.index);
        self.index += 1;
        token
    }
}


type YieldExpr<'a> = Result<(TokenWalker<'a>, Expression), Error>;
type YieldStmt<'a> = Result<(TokenWalker<'a>, Statement), Error>;
pub type YieldStmts = Result<Vec<Statement>, Vec<Error>>;

pub fn parse(tokens: &[SpannedToken]) -> YieldStmts {
    program(TokenWalker::new(tokens))
}

fn program(mut tokens: TokenWalker) -> YieldStmts {
    let mut stmts = vec!{};
    let mut errs = vec!{};

    loop {
        match statement(tokens.clone()) {
            Ok((rest, stmt)) => {
                stmts.push(stmt);
                tokens = rest;
            },
            Err(Error::Unmatched) => break,
            Err(f @ Error::Failure(_)) => {
                errs.push(f);
                error_recovery(&mut tokens);
            },
        }
    }

    if errs.is_empty() {
        Ok(stmts)
    } else {
        Err(errs)
    }
}


fn error_recovery(tokens: &mut TokenWalker) {
    // ignore all tokens up to the next semicolon,
    let start = tokens.peek().map(|x| (*x).clone());
    loop {
        match tokens.next() {
            Some(end) if end.token == Token::Semicolon => {
                println!("skipped from {} to {}",
                    start.unwrap(),
                    end,
                );
                break;
            },
            Some(_) => {},
            None => break,
        }
    }
}

fn statement(tokens: TokenWalker) -> YieldStmt {
    if tokens.is_empty() {
        return Err(Error::Unmatched);
    }

    match print(tokens.clone()) {
        Err(Error::Unmatched) => {},
        x @ (Ok(_) | Err(Error::Failure(_))) => return x,
    }

    match exprstmt(tokens.clone()) {
        Err(Error::Unmatched) => {},
        x @ (Ok(_) | Err(Error::Failure(_))) => return x,
    }

    Err(Error::Unmatched)
}

macro_rules! expect_token {
    ($span:expr, $tokens:expr, $token:path, $ok:expr) => {
        match $tokens.next() {
            Some(st) if st.token == $token => {
                Ok(($tokens, $ok))
            },
            x @ (None | Some(_)) => {
                Err(Error::Failure(format!(
                    "[{:?}] expected {}, got {:?}",
                    $span, stringify!($token), x
                )))
            },
        }
    }
}

fn print(mut tokens: TokenWalker) -> YieldStmt {
    match tokens.next() {
        Some(st) if st.token == Token::Print => {
            match expression(tokens) {
                Ok((mut rest, expr)) => {
                    expect_token!(st.span, rest, Token::Semicolon, Statement::Print(expr))
                },
                Err(Error::Unmatched) => {
                    Err(Error::Failure(format!(
                        "[{:?}] print expected expression",
                        st.span
                    )))
                },
                Err(f @ Error::Failure(_)) => Err(f),
            }
        },
        None | Some(_) => Err(Error::Unmatched),
    }
}

fn exprstmt(tokens: TokenWalker) -> YieldStmt {
    let span = tokens.peek().map(|x| x.span.clone());
    match dbg!(expression(tokens)) {
        Ok((mut rest, expr)) => {
            expect_token!(span, rest, Token::Semicolon, Statement::Expression(expr))
        },
        Err(e) => Err(e),
    }
}

macro_rules! repeat_las_binop {
    ($tokens: expr, $lower:expr, { $($map_from:path => $map_to:path),+ $(,)? }) => {
        match $lower($tokens) {
            Ok((mut outer_rest, mut outer_expr)) => {
                loop {
                    match outer_rest.next() {
                        Some(st @ SpannedToken { token: $($map_from)|+, .. }) => {
                            match $lower(outer_rest) {
                                Ok((inner_rest, inner_expr)) => {
                                    let ex = match st.token {
                                        $($map_from => $map_to),+,
                                        _ => unreachable!(),
                                    };
                                    outer_expr = ex(Box::new(outer_expr), Box::new(inner_expr));
                                    outer_rest = inner_rest;
                                },

                                Err(Error::Unmatched)
                                    => break Err(Error::Failure(format!(
                                        "[{:?}] expected {} expression after {:?}",
                                        st.span, stringify!($lower), st.token
                                    ))),

                                Err(f @ Error::Failure(_)) => break Err(f),
                            }
                        },
                        None | Some(_) => break Ok((outer_rest.back().unwrap(), outer_expr)),
                    }
                }
            },
            Err(e) => Err(e),
        }
    }
}

fn expression(tokens: TokenWalker) -> YieldExpr {
    equality(tokens)
}

fn equality(tokens: TokenWalker) -> YieldExpr {
    // equality := comparison ( ( BangEqual | EqualEqual ) comparison )* ;
    repeat_las_binop!(tokens, comparison, {
        Token::BangEqual => Expression::NotEqual,
        Token::EqualEqual => Expression::Equal,
    })
}

fn comparison(tokens: TokenWalker) -> YieldExpr {
    // comparison := term ( ( Greater | GreaterEqual | Less | LessEqual ) term )* ;
    repeat_las_binop!(tokens, term, {
        Token::Greater => Expression::Greater,
        Token::GreaterEqual => Expression::GreaterEqual,
        Token::Less => Expression::Less,
        Token::LessEqual => Expression::LessEqual,
    })
}

fn term(tokens: TokenWalker) -> YieldExpr {
    // term := factor ( ( Minus | Plus ) factor )* ;
    repeat_las_binop!(tokens, factor, { Token::Minus => Expression::Subtract, Token::Plus => Expression::Add })
}

fn factor(tokens: TokenWalker) -> YieldExpr {
    // factor := unary ( ( Slash | Star ) unary )* ;
    repeat_las_binop!(tokens, unary, { Token::Slash => Expression::Divide, Token::Star => Expression::Multiply })
}

fn unary(mut tokens: TokenWalker) -> YieldExpr {
    // unary := ( Bang | Minus ) unary | primary ;
    match tokens.next() {
        Some(SpannedToken {
            token: t @ (Token::Bang | Token::Minus),
            span
        }) => {
            let ex = match t {
                Token::Bang => Expression::Negate,
                Token::Minus => Expression::Negative,
                _ => unreachable!(),
            };

            match unary(tokens) {
                Ok((rest, expr)) => Ok((rest, ex(Box::new(expr)))),
                Err(Error::Unmatched) => Err(Error::Failure(format!(
                    "[{:?}] expected unary expression after {:?}",
                    span, t
                ))),
                Err(f @ Error::Failure(_)) => Err(f),
            }
        },
        Some(_) => primary(tokens.back().unwrap()),
        None => Err(Error::Unmatched),
    }
}

fn primary(mut tokens: TokenWalker) -> YieldExpr {
    // primary := Number | String | True | False | Nil | LeftParen expression RightParen
    use Token::{
        Number, String, Identifier,
        True, False, Nil,
        LeftParen, RightParen,
    };
    use SpannedToken as St;
    match tokens.next() {
        Some(St { token: Number(n), .. })
            => Ok((tokens, Expression::Number(*n))),
        Some(St { token: String(s), .. })
            => Ok((tokens, Expression::String(s.clone()))),
        Some(St { token: Identifier(i), .. })
            => Ok((tokens, Expression::Identifier(i.clone()))),
        
        Some(st) if st.token == True
            => Ok((tokens, Expression::True)),
        Some(st) if st.token == False
            => Ok((tokens, Expression::False)),
        Some(st) if st.token == Nil
            => Ok((tokens, Expression::Nil)),

        Some(st) if st.token == LeftParen => {
            match expression(tokens) {
                Ok((mut rest, expr)) => {
                    expect_token!(st.span, rest, RightParen, expr)
                },
                Err(Error::Unmatched)
                    => Err(Error::Failure(format!(
                        "[{:?}] found ( not followed by expression",
                        st.span
                    ))),
                Err(f @ Error::Failure(_)) => Err(f),
            }
        }

        Some(_) | None => Err(Error::Unmatched),
    }
}
