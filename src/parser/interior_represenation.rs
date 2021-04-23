use std::rc::Rc;

use crate::tokenizer::token::Token;

pub enum BackwardsToken {
    Lambda(Vec<String>),
    App,
    Func,
    Ann,
    Name(String),
    Command(String),
}

pub enum TokenTree {
    Lam(Vec<String>, Rc<TokenTree>),
    Ann(Rc<TokenTree>, Rc<TokenTree>),
    App(Rc<TokenTree>, Rc<TokenTree>),
    Command(Token, Rc<TokenTree>),
}