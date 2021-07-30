use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
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
    #[regex(r#"[a-zA-Z_][a-zA-Z0-9_]*"#)]
    Identifier(&'a str),
    #[regex(r#"[0-9]+(\.[0-9]+)?"#)]
    Number(&'a str),
    #[regex(r#""([^"\\]|\\[\\"nt])*""#)]
    String(&'a str),

    // Internal tokens
    #[error]
    Error,
    #[regex(r#"//[^\n]*"#, logos::skip)]
    Comment,
    #[regex(r#"[ \t\n]+"#, logos::skip)]
    Whitespace,
}

pub type SpannedToken<'a> = (std::ops::Range<usize>, Token<'a>);

pub fn lex(source: &str) -> Vec<SpannedToken> {
    let mut lexer = Token::lexer(source);
    let mut tokens = vec!{};

    loop {
        if let Some(token) = lexer.next() {
            tokens.push((lexer.span(), token));
        } else {
            break;
        }
    }

    tokens
}