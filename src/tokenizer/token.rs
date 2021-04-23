#[derive(Clone)]
pub struct Token {
    typ: TokenType,
    text: String,
}

impl Token {
    pub fn new(text: &str) -> Result<Token, String> {
        Ok(Token {typ: TokenType::spot(text)?, text: String::from(text)})
    }

    pub fn get_type(&self) -> TokenType {
        self.typ
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    OpenBracket,
    CloseBracket,
    Lambda,
    Arrow,
    App,
    Ann,
    Name,
    Command,
}

impl TokenType {
    pub fn spot(text: &str) -> Result<TokenType, String> {
        match text {
            "" => Ok(TokenType::App),
            "(" => Ok(TokenType::OpenBracket),
            ")" => Ok(TokenType::CloseBracket),
            "\\" => Ok(TokenType::Lambda),
            "->" => Ok(TokenType::Arrow),
            ":" => Ok(TokenType::Ann),
            c if TokenType::is_command(c) => Ok(TokenType::Command),
            n if TokenType::is_name(n) => Ok(TokenType::Name),
            text => Err(format!("Unknown token {:?}", text))
        }
    }

    fn is_command(text: &str) -> bool {
        text == "let"
    }

    fn is_name(text: &str) -> bool {
        text.chars().all(|x| x.is_alphanumeric())
    }
}