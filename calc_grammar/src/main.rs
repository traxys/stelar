use stelar::Extract;

#[derive(Debug)]
enum Operation {
    Plus,
    Minus,
    Times,
    Div,
    Modulo,
    Power,
}

impl Operation {
    fn from_string(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            "*" => Some(Self::Times),
            "/" => Some(Self::Div),
            "%" => Some(Self::Modulo),
            "^" => Some(Self::Power),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Token {
    Integer(u64),
    Float(f64),
    Operation(Operation),
    Skip,
}

impl Token {
    fn is_integer(s: &str) -> bool {
        s.parse::<u64>().is_ok()
    }
    fn is_float(s: &str) -> bool {
        s.parse::<f64>().is_ok()
    }
    fn is_op(s: &str) -> bool {
        match s {
            "+" | "-" | "*" | "/" | "%" | "^" => true,
            _ => false,
        }
    }
    fn is_skip(s: &str) -> bool {
        match s {
            " " => true,
            _ => false,
        }
    }
}

impl Extract<String> for Token {
    type Error = ();
    fn extract(input: &mut String) -> Result<Option<Self>, Self::Error> {
        let mut possible = None;
        let mut max_valid = 1;
        if input.len() == 0 {
            return Ok(None);
        } else if input.len() == 1 {
            if Token::is_skip(input) {
                possible = Some(3);
            }
            if Token::is_op(input) {
                possible = Some(2);
            }
        } else {
            for i in 1..input.len() {
                let mut has_valid = false;
                let sub = &input[0..i];
                max_valid = i;
                if Token::is_float(sub) {
                    has_valid = true;
                    possible = Some(0);
                }
                if Token::is_integer(sub) {
                    has_valid = true;
                    possible = Some(1);
                }
                if Token::is_op(sub) {
                    has_valid = true;
                    possible = Some(2);
                }
                if Token::is_skip(sub) {
                    has_valid = true;
                    possible = Some(3);
                }
                if !has_valid {
                    max_valid = i - 1;
                    break;
                }
            }
        }
        let mut token = input.split_off(max_valid);
        std::mem::swap(&mut token, input);

        Ok(match possible {
            Some(0) => Some(Token::Float(token.parse().unwrap())),
            Some(1) => Some(Token::Integer(token.parse().unwrap())),
            Some(2) => Some(Token::Operation(Operation::from_string(&token).unwrap())),
            Some(3) => Some(Token::Skip),
            None => None,
            _ => unreachable!(),
        })
    }
}

fn main() {
    let mut input = "9 +    -    12.46 *".to_string();
    while let Ok(Some(token)) = Token::extract(&mut input) {
        println!("Token: {:?}", token);
    }
}
