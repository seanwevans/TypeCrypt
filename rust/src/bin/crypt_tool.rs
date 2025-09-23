use std::{env, fmt, num::ParseIntError};
use typecrypt::{decrypt_with_value, encrypt, DecryptError, Type, Value};

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

#[derive(Debug)]
enum CliDecryptError {
    Hex(HexError),
    Decrypt(DecryptError),
}

fn decrypt_from_hex(ty: &Type, ciphertext_hex: &str) -> Result<Vec<u8>, CliDecryptError> {
    let ct = unhex(ciphertext_hex).map_err(CliDecryptError::Hex)?;
    let val = default_value(ty);
    decrypt_with_value(ty, &val, ct.as_slice()).map_err(CliDecryptError::Decrypt)
}

const PLAINTEXT: &[u8] = b"cross-test";

fn parse_type(name: &str) -> Option<Type> {
    let trimmed = name.trim();
    match trimmed {
        "int" => Some(Type::Int),
        "str" => Some(Type::Str),
        "bool" => Some(Type::Bool),
        "pair" => Some(Type::Pair(Box::new(Type::Int), Box::new(Type::Bool))),
        "list" => Some(Type::List(Box::new(Type::Int))),
        _ => {
            if let Some(inner) = trimmed.strip_prefix("list<") {
                inner
                    .strip_suffix('>')
                    .and_then(parse_type)
                    .map(|t| Type::List(Box::new(t)))
            } else {
                None
            }
        }
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
            let ct = match unhex(&args[3]) {
                Ok(bytes) => bytes,
                Err(e) => {
                    eprintln!("{e}");
                    std::process::exit(1);
                }
            };
            let val = default_value(&ty);
            match decrypt_with_value(&ty, &val, &ct) {
                Ok(pt) => println!("{}", String::from_utf8_lossy(&pt)),
                Err(CliDecryptError::Hex(err)) => {
                    eprintln!("invalid ciphertext: {err}");
                    std::process::exit(1);
                }
                Err(CliDecryptError::Decrypt(err)) => {
                    let _ = err;
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

    #[test]
    fn decrypt_reports_invalid_hex_before_attempting() {
        let ty = Type::Int;
        assert!(matches!(
            decrypt_from_hex(&ty, "123"),
            Err(CliDecryptError::Hex(HexError::InvalidLength))
        ));
        assert!(matches!(
            decrypt_from_hex(&ty, "zz"),
            Err(CliDecryptError::Hex(HexError::Parse(_)))
        ));
    }
}
