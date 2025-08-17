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

fn main() -> Result<(), ring::error::Unspecified> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: encrypt|decrypt <hex>");
        std::process::exit(1);
    }
    match args[1].as_str() {
        "encrypt" => {
            let ct = encrypt(&Type::Int, PLAINTEXT)?;
            println!("{}", hex(&ct));
        }
        "decrypt" => {
            if args.len() != 3 {
                eprintln!("decrypt requires hex ciphertext");
                std::process::exit(1);
            }
            let ct = match unhex(&args[2]) {
                Ok(ct) => ct,
                Err(e) => {
                    eprintln!("invalid hex string: {e}");
                    std::process::exit(1);
                }
            };
            let val = Value::Int(0);
            match decrypt_with_value(&Type::Int, &val, &ct) {
                Ok(pt) => println!("{}", String::from_utf8_lossy(&pt)),
                Err(_) => {
                    println!("FAIL");
                    std::process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("usage: encrypt|decrypt <hex>");
            std::process::exit(1);
        }
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
