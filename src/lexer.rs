use bstr::BString;
use logos::Logos;
use logos::SpannedIter;

#[rustfmt::skip]
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+", skip r"#.*\n?", error = LexicalError)]
pub enum Token {
    #[token("fn")] KwFn,
    #[token("var")] KwVar,
    #[token("print")] KwPrint,
    #[token("exit")] KwExit,
    #[token("return")] KwReturn,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| BString::from(lex.slice().to_string()))] Identifier(BString),
    #[regex("[0-9]+", |lex| lex.slice().parse())] Integer(i64),

    #[token("{")] LCurly,
    #[token("}")] RCurly,

    #[token("(")] LParen,
    #[token(")")] RParen,

    #[token("=")] Assign,
    #[token(";")] Semicolon,

    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Mul,
    #[token("/")] Div,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(std::num::ParseIntError),
    #[default]
    InvalidToken,
}

impl From<std::num::ParseIntError> for LexicalError {
    fn from(err: std::num::ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
