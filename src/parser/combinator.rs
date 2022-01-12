use crate::lexer::tokens::Token;

#[derive(Clone, Copy)]
pub struct ParserState<'a> {
    tokens: &'a [Token],
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    pub fn next(self) -> ParseOption<'a, Token> {
        if self.tokens.len() < 1 {
            None
        } else {
            Some((
                // TODO: Avoid this clone
                self.tokens[0].clone(),
                ParserState {
                    tokens: &self.tokens[1..],
                },
            ))
        }
    }

    pub fn advance(self) -> Self {
        let (_, state) = self.next().unwrap();
        state
    }
}

#[derive(Debug)]
pub enum ParseFailure {
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
}

pub type ParseResult<'a, O> = Result<(O, ParserState<'a>), ParseFailure>;

pub type ParseOption<'a, O> = Option<(O, ParserState<'a>)>;

pub fn success<'a, O>(value: O, state: ParserState<'a>) -> ParseResult<'a, O> {
    Ok((value, state))
}

pub fn failure<'a, O>(error: ParseFailure) -> ParseResult<'a, O> {
    Err(error)
}

pub fn expect_token(state: ParserState) -> ParseResult<Token> {
    match state.next() {
        Some((token, state)) => Ok((token, state)),
        None => Err(ParseFailure::UnexpectedEndOfInput),
    }
}
