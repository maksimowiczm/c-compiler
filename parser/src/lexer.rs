use crate::tokenizer::{Token, TokenKind};

pub struct Lexer {
    tokens: Vec<Token>,
    index: usize,
    borrowed: usize,
}

impl Lexer {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            borrowed: Default::default(),
        }
    }

    pub fn borrow(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.index + self.borrowed);
        if token.is_some() {
            self.borrowed += 1;
        }

        token
    }

    pub fn release(&mut self, count: usize) {
        if count > self.borrowed {
            // todo
            panic!("attempted to release more tokens than borrowed");
        }

        self.borrowed -= count;
    }

    pub fn consume(&mut self) {
        self.index += self.borrowed;
        self.borrowed = 0;
    }

    pub fn expect(&mut self, token: TokenKind) -> bool {
        if let Some(t) = self.borrow() {
            if t.kind == token {
                self.consume();
                return true;
            }
        }

        self.release(1);
        false
    }
}
