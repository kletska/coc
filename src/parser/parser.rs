use std::rc::Rc;

use crate::tokenizer::token::Token;
use crate::tokenizer::token::TokenType;
use crate::core::simply_typed::TermDown;
use super::interior_represenation::TokenTree;
use super::interior_represenation::BackwardsToken;

pub struct Parser {}

impl Parser {

    fn parese_term(tree: Rc<TokenTree>) -> Result<Rc<TermDown>, String> {
        todo!()
    }

    fn rec_parse(mut program: Vec<Token>) -> Result<Rc<TokenTree>, String> {
        todo!()
    }

    fn parse_tree(mut program: Vec<Token>) -> Result<Rc<TokenTree>, String> {
        program.reverse();
        Parser::rec_parse(program) 
    }

    fn parse_tokens(program: Vec<Token>) -> Result<Vec<Token>, String> {
        let mut program_with_apps = vec![];

 
        for i in 1..program.len() {
            let first_typ = program[i - 1].get_type();
            let second_typ = program[i] .get_type();

            use TokenType::*;

            program_with_apps.push(program[i].clone());

            if first_typ == CloseBracket || first_typ == Name {
                if second_typ == OpenBracket || second_typ == Name {
                    program_with_apps.push(Token::new("").unwrap());
                }
            }
        }
        program_with_apps.push(program.last().unwrap().clone());

        Ok(program_with_apps)
    }

    pub fn run(program: Vec<Token>) -> Result<Rc<TermDown>,String> {
        Ok(Parser::parese_term(Parser::parse_tree(Parser::parse_tokens(program)?)?)?)
    }
}