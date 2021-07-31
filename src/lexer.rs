use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Arithmetic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    // Boolean operators
    #[token("!")]
    Bang,
    #[token("and")]
    And,
    #[token("or")]
    Or,

    // Comparison operators
    #[token(">")]
    Greater,
    #[token("<")]
    Less,
    #[token("!=")]
    BangEqual,
    #[token("==")]
    EqualEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<=")]
    LessEqual,

    // Assignment operator
    #[token("=")]
    Equal,

    // Control flow
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,

    // Conditionals
    #[token("if")]
    If,
    #[token("else")]
    Else,

    // Loops
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,

    // Classes
    #[token("class")]
    Class,
    #[token("this")]
    This,
    #[token("super")]
    Super,

    // Functions
    #[token("fun")]
    Fun,
    #[token("return")]
    Return,

    // Variables
    #[token("var")]
    Var,

    // Print
    #[token("print")]
    Print,

    // Literals
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("nil")]
    Nil,

    #[regex(
        r#"[a-zA-Z_][a-zA-Z0-9_]*"#,
        |lex| lex.slice().to_string()
    )]
    Identifier(String),

    #[regex(
        r#""([^"\\]|\\[\\"nt])*""#,
        |lex| {
            let s = lex.slice();
            // TODO: proper escaping
            s[1..s.len() - 1].to_string()
        }
    )]
    String(String),

    #[regex(
        r#"[0-9]+(\.[0-9]+)?"#,
        |lex| lex.slice().parse()
    )]
    Number(f64),

    // Internal tokens
    #[error]
    Error,
    #[regex(r#"//[^\n]*"#, logos::skip)]
    Comment,
    #[regex(r#"[ \t\n]+"#, logos::skip)]
    Whitespace,
}

pub type Span = std::ops::Range<usize>;

// pub type SpannedToken = (Span, Token);

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub span: Span,
    pub token: Token,
}

impl std::fmt::Display for SpannedToken {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            fmt,
            "[{} {:?}]",
            self.span.start,
            // self.span.end,
            self.token
        )
    }
}


pub fn lex(source: &str) -> Vec<SpannedToken> {
    let mut lexer = Token::lexer(source);
    let mut tokens = vec!{};

    while let Some(token) = lexer.next() {
        tokens.push(SpannedToken {
            span: lexer.span(),
            token
        });
    }

    tokens
}