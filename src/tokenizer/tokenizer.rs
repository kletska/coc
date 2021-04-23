use super::token::Token;

pub struct Tokenizer {}

const separating_symbols: &[&'static str] = &[":", "->", "(", ")", "\\"];

impl Tokenizer {
    pub fn run(text: String) -> Result<Vec<Token>, String> {
        let all_errors: Vec<Result<Token, String>> = separating_symbols
            .iter()
            .fold(text, |text, ch| {
                text.replace(ch, &format!(" {} ", ch)[..])
            })
            .split_whitespace()
            .map(Token::new)
            .collect();
        all_errors.into_iter().collect()
    }
}