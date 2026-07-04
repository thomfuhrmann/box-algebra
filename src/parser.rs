use crate::{BoxValue, BoxVariant, Color, store::BoxStore};

use logos::{Lexer, Logos};
use malachite::Natural;

fn positive(lex: &mut Lexer<Token>) -> Option<Natural> {
    let slice = lex.slice();
    let n = slice.parse().ok()?;
    Some(n)
}

fn negative(lex: &mut Lexer<Token>) -> Option<Natural> {
    let slice = lex.slice();
    let n = slice[1..].parse().ok()?;
    Some(n)
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    // Match numbers
    #[regex(r"[0-9]+", positive)]
    Pos(Natural),
    // Match negative numbers
    #[regex(r"-[0-9]+", negative)]
    Neg(Natural),
    // Match variables like 'alpha'
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[token("+")]
    Plus,
    #[token("*")]
    Asterisk,
    #[token("^")]
    Caret,
}

impl Token {
    fn get_binding_power(&self) -> (u8, u8) {
        match self {
            Token::Plus => (1, 2),
            Token::Asterisk => (3, 4),
            Token::Caret => (6, 5),
            _ => (0, 0),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Mul,
    Pow,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Pos(Natural),
    Neg(Natural),
    Variable(String),
    BinaryOp {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
enum ParserState {
    Token(Token),
    Error,
    Eof,
}

#[derive(Debug)]
pub struct Parser<'source> {
    lexer: Lexer<'source, Token>,
    peeked: Option<ParserState>,
}

impl<'source> Parser<'source> {
    /// Create a new parser from source
    pub fn new(source: &'source str) -> Self {
        Self {
            lexer: Token::lexer(source),
            peeked: None,
        }
    }

    /// Internal helper to fill the peek cache if it's empty
    fn fill_cache(&mut self) {
        if self.peeked.is_none() {
            let next_state = match self.lexer.next() {
                Some(Ok(token)) => ParserState::Token(token),
                Some(Err(_)) => ParserState::Error,
                None => ParserState::Eof,
            };
            self.peeked = Some(next_state);
        }
    }

    /// Look ahead at the next state without moving the cursor
    fn peek(&mut self) -> ParserState {
        self.fill_cache();
        self.peeked.clone().unwrap()
    }

    /// Consume the token and move forward
    fn advance(&mut self) -> ParserState {
        self.fill_cache();
        self.peeked.take().unwrap()
    }
}

impl<'source> Parser<'source> {
    pub fn parse(&mut self) -> Expr {
        self.parse_expr(0)
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        // parse prefix (must be a value, not an operator)
        let mut expr = match self.advance() {
            ParserState::Token(Token::Pos(n)) => Expr::Pos(n),
            ParserState::Token(Token::Neg(n)) => Expr::Neg(n),
            ParserState::Token(Token::Identifier(s)) => Expr::Variable(s),
            ParserState::Error => panic!("Lexer encountered an invalid token"),
            ParserState::Eof => panic!("Syntax Error: Unexpected EOF"),
            _ => panic!("Syntax Error: Expected an atomic expression"),
        };

        // process infix operations
        loop {
            let next = self.peek();

            let token = match next {
                ParserState::Token(tok) => tok,
                ParserState::Eof | ParserState::Error => break,
            };

            let (left_bp, right_bp) = token.get_binding_power();
            if left_bp < min_bp {
                break;
            }

            // We know it's an operator, consume it securely
            let op = match self.advance() {
                ParserState::Token(Token::Plus) => Op::Add,
                ParserState::Token(Token::Asterisk) => Op::Mul,
                ParserState::Token(Token::Caret) => Op::Pow,
                _ => unreachable!(),
            };

            let right = self.parse_expr(right_bp);

            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        expr
    }
}

impl Expr {
    pub fn eval(self, store: &BoxStore) -> BoxVariant {
        match self {
            Expr::Pos(n) => {
                // Convert raw number
                BoxVariant::Num(BoxValue::from(n))
            }
            Expr::Neg(n) => {
                // Convert raw negative number
                let mut num = BoxValue::from(n);
                num.set_color(1, Color::Red);
                BoxVariant::Num(num)
            }
            Expr::Variable(name) => store
                .fetch_box_by_name(&name)
                .expect("Undefined variable assignment"),
            Expr::BinaryOp { op, left, right } => {
                let left_val = left.eval(store);
                let right_val = right.eval(store);

                match op {
                    Op::Add => match (left_val, right_val) {
                        (BoxVariant::Num(l), BoxVariant::Num(r)) => BoxVariant::Num(l + r),
                        (BoxVariant::Num(l), BoxVariant::Polynum(r)) => BoxVariant::Polynum(l + r),
                        (BoxVariant::Polynum(l), BoxVariant::Num(r)) => BoxVariant::Polynum(l + r),
                        (BoxVariant::Polynum(l), BoxVariant::Polynum(r)) => {
                            BoxVariant::Polynum(l + r)
                        }
                        (l, r) => panic!("Type Error: Cannot add variant {:?} to {:?}", l, r),
                    },
                    Op::Mul => match (left_val, right_val) {
                        (BoxVariant::Num(l), BoxVariant::Num(r)) => BoxVariant::Num(l * r),
                        (BoxVariant::Num(l), BoxVariant::Polynum(r)) => BoxVariant::Polynum(l * r),
                        (BoxVariant::Polynum(l), BoxVariant::Num(r)) => BoxVariant::Polynum(l * r),
                        (BoxVariant::Polynum(l), BoxVariant::Polynum(r)) => {
                            BoxVariant::Polynum(l * r)
                        }
                        (l, r) => panic!("Type Error: Cannot add variant {:?} to {:?}", l, r),
                    },
                    Op::Pow => {
                        todo!()
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{BoxValue, parser::Parser, store::BoxStore};

    #[test]
    fn test_parse() {
        let mut store = BoxStore::new();
        let alpha = BoxValue::alpha();
        store.store_box_with_name("alpha", alpha);
        let input = "-2 + -2*alpha + 5*alpha*alpha";
        let mut parser = Parser::new(input);
        let expr = parser.parse();
        let val = expr.eval(&store).into_any();

        println!("{:#}", val);
    }
}
