use crate::{BoxValue, BoxVariant, store::BoxStore};

use chumsky::prelude::*;
use logos::{Lexer, Logos};
use malachite::Natural;

fn parse_subscript(lex: &mut Lexer<Token>) -> Option<Natural> {
    let slice = lex.slice();
    let mut result = Natural::from(0_u32);

    for ch in slice.chars() {
        let digit: u32 = match ch {
            '₀' => 0,
            '₁' => 1,
            '₂' => 2,
            '₃' => 3,
            '₄' => 4,
            '₅' => 5,
            '₆' => 6,
            '₇' => 7,
            '₈' => 8,
            '₉' => 9,
            _ => return None,
        };

        let digit = Natural::from(digit);
        let base = Natural::from(10_u32);

        result = base * result + digit;
    }

    Some(result)
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    // Match numbers
    #[regex(r"[0-9]+", |lex|lex.slice().parse())]
    Number(Natural),
    // Match Vars like 'alpha'
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Var(String),
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("^")]
    Caret,
    #[token("(")]
    OpenGroup,
    #[token(")")]
    CloseGroup,
    #[token("⎣")]
    OpenBox,
    #[token("⎦")]
    CloseBox,
    #[token("⎡")]
    OpenList,
    #[token("⎤")]
    CloseList,
    #[token(",")]
    Comma,
    #[regex(r"[₀₁₂₃₄₅₆₇₈₉]+", parse_subscript)]
    Subscript(Natural),
    #[token("⧠")]
    Box,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(Natural),
    Var(String),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Unixel(Box<Expr>),
    Vexel(Vec<Expr>),
    Pixel(Box<Expr>, Box<Expr>),
    Maxel(Vec<Expr>),
    List(Vec<Expr>),
    Box(Vec<Expr>),
    Subscript(Natural, Box<Expr>),
}

pub fn parser<'src>()
-> impl Parser<'src, &'src [Token], Expr, chumsky::extra::Err<chumsky::error::Simple<'src, Token>>>
{
    recursive(|p| {
        let atom = {
            let number = select! {
                Token::Number(n) => Expr::Num(n),
            };

            let var = select! { Token::Var(name) => Expr::Var(name) };

            let subscript_prefix =
                any::<&[Token], extra::Err<Simple<Token>>>().filter_map(|token| match token {
                    Token::Subscript(num) => Some(num),
                    _ => None,
                });

            let unixel = p
                .clone()
                .delimited_by(just(Token::OpenList), just(Token::CloseList))
                .map(|v| Expr::Unixel(Box::new(v)));

            let unixel_with_subscript =
                subscript_prefix
                    .or_not()
                    .then(unixel)
                    .map(|(sub, expr)| match sub {
                        Some(num) => Expr::Subscript(num, Box::new(expr)),
                        None => expr,
                    });

            let vexel = unixel_with_subscript
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenBox), just(Token::CloseBox))
                .map(Expr::Vexel);

            let pixel = p
                .clone()
                .then_ignore(just(Token::Comma))
                .then(p.clone())
                .delimited_by(just(Token::OpenList), just(Token::CloseList))
                .map(|(left, right)| Expr::Pixel(Box::new(left), Box::new(right)));

            let pixel_with_subscript =
                subscript_prefix
                    .or_not()
                    .then(pixel)
                    .map(|(sub, expr)| match sub {
                        Some(num) => Expr::Subscript(num, Box::new(expr)),
                        None => expr,
                    });

            let maxel = pixel_with_subscript
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenBox), just(Token::CloseBox))
                .map(Expr::Maxel);

            let list = subscript_prefix
                .or_not()
                .then(p.clone())
                .map(|(sub, expr)| match sub {
                    Some(num) => Expr::Subscript(num, Box::new(expr)),
                    None => expr,
                })
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenList), just(Token::CloseList))
                .map(Expr::List);

            let any_box = subscript_prefix
                .or_not()
                .then(p.clone())
                .map(|(sub, expr)| match sub {
                    Some(num) => Expr::Subscript(num, Box::new(expr)),
                    None => expr,
                })
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::OpenBox), just(Token::CloseBox))
                .map(Expr::Box);

            let parenthesized = p
                .clone()
                .delimited_by(just(Token::OpenGroup), just(Token::CloseGroup));

            let base_atom = number
                .or(var)
                .or(vexel)
                .or(maxel)
                .or(list)
                .or(any_box)
                .or(parenthesized);

            just(Token::Minus)
                .repeated()
                .collect::<Vec<_>>()
                .then(base_atom)
                .map(|(minuses, mut expr)| {
                    for _ in minuses {
                        expr = Expr::Neg(Box::new(expr));
                    }
                    expr
                })
        };

        let prod = atom.clone().foldl(
            just(Token::Multiply)
                .or(just(Token::Divide))
                .then(atom)
                .repeated(),
            |lhs, (op, rhs)| match op {
                Token::Multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                Token::Divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            },
        );

        prod.clone().foldl(
            just(Token::Plus)
                .or(just(Token::Minus))
                .then(prod)
                .repeated(),
            |lhs, (op, rhs)| match op {
                Token::Plus => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Token::Minus => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            },
        )
    })
}

impl Expr {
    pub fn eval(&self, store: &BoxStore) -> BoxVariant {
        match self {
            Expr::Subscript(n, v) => {
                let mut variant = v.eval(store);
                variant.set_multiplicity(0, n.clone());
                variant
            }
            Expr::Num(n) => BoxVariant::Num(BoxValue::from(n.clone())),
            Expr::Neg(rhs) => BoxVariant::Num(BoxValue::from(-1)) * rhs.eval(store),
            Expr::Add(lhs, rhs) => lhs.eval(store) + rhs.eval(store),
            Expr::Mul(lhs, rhs) => lhs.eval(store) * rhs.eval(store),
            Expr::Sub(lhs, rhs) => {
                lhs.eval(store) + BoxVariant::Num(BoxValue::from(-1)) * rhs.eval(store)
            }
            // Expr::Div(lhs, rhs) => todo!(),
            Expr::Var(name) => store
                .fetch_box_by_name(name)
                .expect("Undefined Var assignment"),
            Expr::Unixel(x) => BoxVariant::Unixel(BoxValue::unixel(x.eval(store).into_any_raw())),
            Expr::Vexel(xs) => {
                let mut vs = Vec::new();
                for x in xs {
                    let variant = x.eval(store);
                    match variant {
                        BoxVariant::Unixel(v) => vs.push(v),
                        _ => unreachable!(),
                    }
                }
                BoxVariant::Vexel(vs.into())
            }
            Expr::Pixel(x, y) => BoxVariant::Pixel(BoxValue::pixel(
                x.eval(store).into_any_raw(),
                y.eval(store).into_any_raw(),
            )),
            Expr::Maxel(pxs) => {
                let mut vs = Vec::new();
                for px in pxs {
                    let variant = px.eval(store);
                    match variant {
                        BoxVariant::Pixel(px) => vs.push(px),
                        _ => unreachable!(),
                    }
                }

                BoxVariant::Maxel(vs.into())
            }
            Expr::Box(bxs) => {
                let mut vs = Vec::new();
                for bx in bxs {
                    let var = bx.eval(store).into_any();
                    vs.push(var.into_any_raw());
                }
                BoxVariant::Any(vs.into())
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::{
        BoxValue,
        parser::{Parser, Token, parser},
        store::BoxStore,
    };

    #[test]
    fn test_parse() {
        let mut store = BoxStore::new();
        let alpha = BoxValue::alpha();
        store.store_box_with_name("alpha", alpha);

        let input = "-2 + 3 - 2*alpha + 5*alpha*alpha";
        let lexer = Token::lexer(input);
        let mut tokens = vec![];
        for (token, span) in lexer.spanned() {
            match token {
                Ok(token) => tokens.push(token),
                Err(e) => {
                    println!("lexer error at {:?}: {:?}", span, e);
                    return;
                }
            }
        }

        // parse the tokens to construct an AST
        let ast = match parser().parse(&tokens).into_result() {
            Ok(expr) => {
                println!("[AST]\n{:#?}", expr);
                expr
            }
            Err(e) => {
                println!("parse error: {:#?}", e);
                return;
            }
        };

        // evaluates the AST to get the result
        let val = ast.eval(&store);
        println!("\n[result]\n{:#}", val);

        let input = "⎣₂⎡2,3⎤,⎡4,6⎤⎦";
        // let input = "⎣⎡2⎤,⎡4⎤,⎡3⎤,⎡6⎤⎦";
        // let input = "⎡2,3,4,5⎤";
        // let input = "⎣₂⎣2,3⎦,⎣4,6⎦⎦";
        let lexer = Token::lexer(input);
        let mut tokens = vec![];
        for (token, span) in lexer.spanned() {
            match token {
                Ok(token) => tokens.push(token),
                Err(e) => {
                    println!("lexer error at {:?}: {:?}", span, e);
                    return;
                }
            }
        }

        // parse the tokens to construct an AST
        let ast = match parser().parse(&tokens).into_result() {
            Ok(expr) => {
                println!("[AST]\n{:#?}", expr);
                expr
            }
            Err(e) => {
                println!("parse error: {:#?}", e);
                return;
            }
        };

        // evaluates the AST to get the result
        let val = ast.eval(&store);

        println!("\n[result]\n{:#}", val);
    }
}
