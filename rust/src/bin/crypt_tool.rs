use std::{env, fmt, num::ParseIntError};
use typecrypt::{decrypt_with_value, encrypt, Type, Value};

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

#[derive(Debug)]
enum HexError {
    InvalidLength,
    Parse(ParseIntError),
}

impl fmt::Display for HexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HexError::InvalidLength => write!(f, "hex string must have even length"),
            HexError::Parse(e) => write!(f, "{e}"),
        }
    }
}

fn unhex(s: &str) -> Result<Vec<u8>, HexError> {
    if s.len() % 2 != 0 {
        return Err(HexError::InvalidLength);
    }
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).map_err(HexError::Parse))
        .collect()
}

const PLAINTEXT: &[u8] = b"cross-test";

fn parse_type(name: &str) -> Option<Type> {
    match name {
        "int" => Some(Type::Int),
        "str" => Some(Type::Str),
        "pair" => Some(Type::Pair(Box::new(Type::Int), Box::new(Type::Bool))),
        _ => None,
    }
}

fn default_value(ty: &Type) -> Value {
    match ty {
        Type::Int => Value::Int(0),
        Type::Str => Value::Str(String::new()),
        Type::Bool => Value::Bool(false),
        Type::Pair(a, b) => Value::Pair(Box::new(default_value(a)), Box::new(default_value(b))),
        Type::List(_) => Value::List(vec![]),
    }
}

fn usage() -> ! {
    eprintln!("usage: encrypt|decrypt <type> [hex]");
    std::process::exit(1);
}

fn main() -> Result<(), ring::error::Unspecified> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        usage();
    }
    match args[1].as_str() {
        "encrypt" => {
            if args.len() != 3 {
                usage();
            }
            let ty = parse_type(&args[2]).unwrap_or_else(|| usage());
            let ct = encrypt(&ty, PLAINTEXT)?;
            println!("{}", hex(&ct));
        }
        "decrypt" => {
            if args.len() != 4 {
                usage();
            }

            let ty = parse_type(&args[2]).unwrap_or_else(|| usage());
            let ct = unhex(&args[3]);
            let val = default_value(&ty);
            match decrypt_with_value(&ty, &val, &ct) {

                Ok(pt) => println!("{}", String::from_utf8_lossy(&pt)),
                Err(_) => {
                    println!("FAIL");
                    std::process::exit(1);
                }
            }
        }
        _ => usage(),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unhex_invalid_string_errors() {
        assert!(unhex("zz").is_err());
        assert!(unhex("123").is_err());
    }
}
