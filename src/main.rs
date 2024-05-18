use std::{fmt::Display, iter::Peekable, str::Chars};

pub type Result<T> = std::result::Result<T, ExprError>;

#[derive(Debug)]
pub enum ExprError {
    Parse(String),
}

impl std::error::Error for ExprError {}

impl Display for ExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(s) => write!(f, "{}", s),
        }
    }
}

enum AssocType {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Token {
    Number(i32),
    Plus,             // 加
    Minus,            // 减
    Multiply,         // 乘
    Divide,           // 除
    Power,            // 幂
    LeftParenthesis,  // 左括号
    RightParenthesis, // 右括号
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Power => write!(f, "^"),
            Self::LeftParenthesis => write!(f, "("),
            Self::RightParenthesis => write!(f, ")"),
        }
    }
}

impl Token {
    // 判断是不是运算符
    fn is_operator(&self) -> bool {
        match self {
            Token::Plus | Token::Minus | Token::Multiply | Token::Divide | Token::Power => true,
            _ => false,
        }
    }

    // 获取运算符优先级
    fn precedence(&self) -> i32 {
        match self {
            Token::Plus | Token::Minus => 1,
            Token::Multiply | Token::Divide => 2,
            Token::Power => 3,
            _ => 0,
        }
    }

    // 获取运算符结合性
    fn assoc(&self) -> AssocType {
        match self {
            Token::Power => AssocType::Right,
            _ => AssocType::Left,
        }
    }

    // 根据当前运算符计算
    fn comput(&self, l: i32, r: i32) -> Option<i32> {
        match self {
            Token::Plus => Some(l + r),
            Token::Minus => Some(l - r),
            Token::Multiply => Some(l * r),
            Token::Divide => Some(l / r),
            Token::Power => Some(l.pow(r as u32)),
            _ => None,
        }
    }
}

// 将一个算术表达式解析成连续的 Token
// 通过 Iterator 返回，或 Peekable 接口获取
struct Tokenizer<'a> {
    // Peekable 其允许查看迭代器中的下一个元素，而不会消费它
    tokens: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(expr: &'a str) -> Self {
        Self {
            tokens: expr.chars().peekable(),
        }
    }

    // 消除字符串前面的空白字符
    fn consume_while(&mut self) {
        while let Some(c) = self.tokens.peek() {
            if c.is_whitespace() {
                self.tokens.next();
            } else {
                break;
            }
        }
    }

    fn scan_number(&mut self) -> Option<Token> {
        let mut num = String::new();
        while let Some(&c) = self.tokens.peek() {
            if c.is_numeric() {
                num.push(c);
                // 消费一个迭代器中的元素
                self.tokens.next();
            } else {
                break;
            }
        }

        match num.parse() {
            Ok(n) => Some(Token::Number(n)),
            Err(_) => None,
        }
    }

    fn scan_operator(&mut self) -> Option<Token> {
        match self.tokens.next() {
            Some('+') => Some(Token::Plus),
            Some('-') => Some(Token::Minus),
            Some('*') => Some(Token::Multiply),
            Some('/') => Some(Token::Divide),
            Some('^') => Some(Token::Power),
            Some('(') => Some(Token::LeftParenthesis),
            Some(')') => Some(Token::RightParenthesis),
            _ => None,
        }
    }
}

// 实现 Iterator，使 Tokenizer 可以通过 for 循环遍历
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume_while();
        // 解析当前 token 类型
        match self.tokens.peek() {
            Some(c) if c.is_numeric() => self.scan_number(),
            Some(_) => self.scan_operator(),
            None => None,
        }
    }
}

struct Expr<'a> {
    iter: Peekable<Tokenizer<'a>>,
}

impl<'a> Expr<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            iter: Tokenizer::new(src).peekable(),
        }
    }

    // 获取结果
    fn eval(&mut self) -> Result<i32> {
        let ret = self.comput_expr(1)?;
        if self.iter.peek().is_some() {
            return Err(ExprError::Parse("Unexpected end of expr".into()));
        }
        Ok(ret)
    }

    // 计算单个 token 或表达式
    fn comput_atom(&mut self) -> Result<i32> {
        match self.iter.peek() {
            Some(Token::Number(n)) => {
                let val = *n;
                self.iter.next();
                Ok(val)
            }
            Some(Token::LeftParenthesis) => {
                self.iter.next();
                let ret = self.comput_expr(1)?;
                match self.iter.next() {
                    Some(Token::RightParenthesis) => (),
                    _ => return Err(ExprError::Parse("Unexpected character".into())),
                };
                Ok(ret)
            }
            _ => Err(ExprError::Parse(
                "Expecting a number or left parenthesis".into(),
            )),
        }
    }

    fn comput_expr(&mut self, min_prec: i32) -> Result<i32> {
        // 计算第一个 Token
        let mut atom_lhs = self.comput_atom()?;

        loop {
            let cur_token = self.iter.peek();
            if cur_token.is_none() {
                break;
            }

            let token = *cur_token.unwrap();

            if !token.is_operator() || token.precedence() < min_prec {
                break;
            }

            let mut next_prec = token.precedence();

            next_prec += match token.assoc() {
                AssocType::Left => 1,
                AssocType::Right => 0,
            };

            self.iter.next();
            let atom_rhs = self.comput_expr(next_prec)?;

            match token.comput(atom_lhs, atom_rhs) {
                Some(ret) => atom_lhs = ret,
                None => return Err(ExprError::Parse("Unexpected expr".into())),
            }
        }
        Ok(atom_lhs)
    }
}

fn main() {
    let src = "1 + 2 + 3 + 4 + 5 ^ 2 / 5  + (3 * 4) / 2 + (23 - 2) / 3";
    let mut expr = Expr::new(src);
    let ret = expr.eval().unwrap();
    println!("{} = {:?}", src, ret);
}

#[test]
fn test_tokenizer() {
    let expr = "1 + 2 * 3 * (4 + 5)";
    let mut tokenizer = Tokenizer::new(expr);
    assert_eq!(tokenizer.next(), Some(Token::Number(1)));
    assert_eq!(tokenizer.next(), Some(Token::Plus));
    assert_eq!(tokenizer.next(), Some(Token::Number(2)));
    assert_eq!(tokenizer.next(), Some(Token::Multiply));
    assert_eq!(tokenizer.next(), Some(Token::Number(3)));
    assert_eq!(tokenizer.next(), Some(Token::Multiply));
    assert_eq!(tokenizer.next(), Some(Token::LeftParenthesis));
    assert_eq!(tokenizer.next(), Some(Token::Number(4)));
    assert_eq!(tokenizer.next(), Some(Token::Plus));
    assert_eq!(tokenizer.next(), Some(Token::Number(5)));
    assert_eq!(tokenizer.next(), Some(Token::RightParenthesis));
    assert_eq!(tokenizer.next(), None);
}
