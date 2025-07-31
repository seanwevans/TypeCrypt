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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: encrypt|decrypt <hex>");
        std::process::exit(1);
    }
    match args[1].as_str() {
        "encrypt" => {
            let ct = encrypt(&Type::Int, PLAINTEXT);
            println!("{}", hex(&ct));
        }
        "decrypt" => {
            if args.len() != 3 {
                eprintln!("decrypt requires hex ciphertext");
                std::process::exit(1);
            }
            let ct = unhex(&args[2]);
            let val = Value::Int(0);
            match decrypt_with_value(&Type::Int, &val, &ct) {
                Some(pt) => println!("{}", String::from_utf8_lossy(&pt)),
                None => {
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
}
