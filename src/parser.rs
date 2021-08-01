use super::lexer::{Token, SpannedToken};

#[derive(Debug)]
pub enum Declaration {
    Variable(String, Expression),

    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    Print(Expression),
    Assignment(String, Expression),

    Expression(Expression),
}

pub type Expr = Box<Expression>;

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

impl std::fmt::Display for Declaration {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Declaration::*;
        match self {
            Variable(s, e) => write!(fmt, "var {} = {};", s, e),

            Statement(s) => s.fmt(fmt),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Statement::*;
        match self {
            Print(e) => write!(fmt, "print {};", e),
            Assignment(s, e) => write!(fmt, "{} = {};", s, e),

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
    pub fn new(tokens: &'a [SpannedToken]) -> TokenWalker {
        TokenWalker {
            tokens: tokens,
            index: 0,
        }
    }

    fn next(&mut self) -> Option<&'a SpannedToken> {
        let token = self.tokens.get(self.index);
        self.index += 1;
        token
    }

    pub fn peek(&self) -> Option<&'a SpannedToken> {
        self.tokens.get(self.index)
    }

    pub fn skip(&mut self) {
        self.index += 1;
    }

    pub fn is_empty(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn len(&self) -> usize {
        if self.index >= self.tokens.len() {
            0
        } else {
            self.tokens.len() - self.index
        }
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

// type YieldToken<'a> = Result<(TokenWalker<'a>, &'a SpannedToken), Error>;
type YieldExpr<'a> = Result<(TokenWalker<'a>, Expression), Error>;
type YieldStmt<'a> = Result<(TokenWalker<'a>, Statement), Error>;
type YieldDecl<'a> = Result<(TokenWalker<'a>, Declaration), Error>;
pub type YieldDecls = Result<Vec<Declaration>, Vec<Error>>;


macro_rules! attempt {
    ($x:expr) => {
        match $x {
            Err(Error::Unmatched) => {},
            x => return x,
        }
    }
}


macro_rules! require_token {
    ($st:expr, $tokens:expr, $token:pat) => {{
        let st: &SpannedToken = $st;
        let mut tokens: TokenWalker = $tokens;

        match tokens.next() {
            Some(st @ SpannedToken { token: $token, .. }) => {
                Ok((tokens, st))
            },
            x @ (None | Some(_)) => {
                Err(Error::Failure(format!(
                    "[{}] expected {}, got {:?}",
                    st, stringify!($token), x
                )))
            },
        }
    }}
}

macro_rules! optional_token {
    ($tokens:expr, $token:pat) => {{
        let mut tokens: TokenWalker = $tokens;        

        match tokens.peek() {
            Some(st @ SpannedToken { token: $token, .. }) => {
                tokens.skip();
                Ok((tokens, st))
            },
            _ => Err(tokens),
        }
    }}
}


fn require<T>(st: &SpannedToken, res: Result<T, Error>, expected: &str) -> Result<T, Error> {
    match res {
        Ok(x) => Ok(x),
        Err(f @ Error::Failure(_)) => Err(f),
        Err(Error::Unmatched) => Err(Error::Failure(format!(
            "[{}] expected {}", st, expected
        ))),
    }
}

macro_rules! try_match_token {
    ($token:pat, $tokens:expr) => {{
        let mut tokens = $tokens;
        match tokens.next() {
            Some(st @ SpannedToken { token: $token, .. }) => Ok((tokens, st)),
            None | Some(_) => Err(Error::Unmatched),
        }
    }}
}



pub fn parse(tokens: &[SpannedToken]) -> YieldDecls {
    program(TokenWalker::new(tokens))
}

fn program(mut tokens: TokenWalker) -> YieldDecls {
    let mut decls = vec!{};
    let mut errs = vec!{};

    loop {
        match declaration(tokens.clone()) {
            Ok((rest, decl)) => {
                decls.push(decl);
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
        Ok(decls)
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

fn declaration(tokens: TokenWalker) -> YieldDecl {
    attempt!(declare(tokens.clone()));
    attempt!(stmtdecl(tokens.clone()));

    Err(Error::Unmatched)
}

fn declare(tokens: TokenWalker) -> YieldDecl {
    let (rest, st) = try_match_token!(Token::Var, tokens)?;
    let (rest, id) = try_match_token!(Token::Identifier(_), rest)?;

    let (rest, expr) = match optional_token!(rest, Token::Equal) {
        Ok((rest, st)) => {
            require(st, expression(rest), "expression")?
        },
        Err(rest) => (rest, Expression::Nil)
    };

    let id = match &id.token {
        Token::Identifier(id) => id.to_string(),
        _ => unreachable!(),
    };
    let decl = Declaration::Variable(id, expr);
    let (rest, _) = require_token!(st, rest, Token::Semicolon)?;
    Ok((rest, decl))
}

fn stmtdecl(tokens: TokenWalker) -> YieldDecl {
    let (rest, stmt) = statement(tokens)?;
    let decl = Declaration::Statement(stmt);
    Ok((rest, decl))
}

fn statement(tokens: TokenWalker) -> YieldStmt {
    attempt!(print(tokens.clone()));
    attempt!(assign(tokens.clone()));
    attempt!(exprstmt(tokens.clone()));

    Err(Error::Unmatched)
}


fn print(tokens: TokenWalker) -> YieldStmt {
    let (rest, st) = try_match_token!(Token::Print, tokens)?;
    let (rest, expr) = require(st, expression(rest), "expression")?;
    let stmt = Statement::Print(expr);
    let (rest, _) = require_token!(st, rest, Token::Semicolon)?;
    Ok((rest, stmt))
}

fn assign(tokens: TokenWalker) -> YieldStmt {
    let (rest, id) = try_match_token!(Token::Identifier(_), tokens)?;
    let (rest, st) = try_match_token!(Token::Equal, rest)?;
    let (rest, expr) = require(st, expression(rest), "expression")?;

    let id = match &id.token {
        Token::Identifier(id) => id.to_string(),
        _ => unreachable!(),
    };
    let stmt = Statement::Assignment(id, expr);
    let (rest, _) = require_token!(st, rest, Token::Semicolon)?;
    Ok((rest, stmt))
}

fn exprstmt(tokens: TokenWalker) -> YieldStmt {
    let st = tokens.peek().map(|x| (*x).clone());
    let (rest, expr) = expression(tokens)?;
    let stmt = Statement::Expression(expr);
    let (rest, _) = require_token!(&st.unwrap(), rest, Token::Semicolon)?;
    Ok((rest, stmt))
}

macro_rules! repeat_las_binop {
    ($tokens: expr, $lower:expr, { $($map_from:pat => $map_to:path),+ $(,)? }) => {{
        let (mut outer_rest, mut outer_expr) = $lower($tokens)?;
        
        while let Some(st @ SpannedToken { token: $($map_from)|+, .. }) = outer_rest.next() {
            let (inner_rest, inner_expr) = require(st, $lower(outer_rest), stringify!($lower))?;

            let ex = match st.token {
                $($map_from => $map_to),+,
                _ => unreachable!(),
            };

            outer_rest = inner_rest;
            outer_expr = ex(Box::new(outer_expr), Box::new(inner_expr));
        }

        Ok((outer_rest.back().unwrap(), outer_expr))
    }}
}

macro_rules! repeat_ras_monop {
    ($self: expr, $tokens: expr, $lower:expr, { $($map_from:pat => $map_to:path),+ $(,)? }) => {{
        match $tokens.next() {
            Some(st @ SpannedToken { token: $($map_from)|+, .. }) => {
                let (rest, expr) = require(st, $self($tokens), stringify!($self))?;

                let ex = match st.token {
                    $($map_from => $map_to),+,
                    _ => unreachable!(),
                };

                Ok((rest, ex(Box::new(expr))))
            },
            Some(_) => $lower($tokens.back().unwrap()),
            None => Err(Error::Unmatched),
        }
    }}
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
    repeat_ras_monop!(unary, tokens, primary, { Token::Bang => Expression::Negate, Token::Minus => Expression::Negative })
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
            let (rest, expr) = require(st, expression(tokens), "expression")?;
            let (rest, _) = require_token!(st, rest, RightParen)?;
            Ok((rest, expr))
        }

        Some(_) | None => Err(Error::Unmatched),
    }
}
