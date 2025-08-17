use std::env;
use typecrypt::{decrypt_with_value, encrypt, Type, Value};

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

fn unhex(s: &str) -> Vec<u8> {
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
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
