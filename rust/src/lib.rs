//! TypeCrypt Rust (Production Branch)
//!
//! This crate will implement the hardened TypeCrypt engine.

#[derive(Debug)]
pub enum Type {
    Int,
    Str,
    Bool,
    Pair(Box<Type>, Box<Type>),
    List(Box<Type>),
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Str(String),
    Bool(bool),
    Pair(Box<Value>, Box<Value>),
    List(Vec<Value>),
}

pub fn matches(value: &Value, ty: &Type) -> bool {
    match (value, ty) {
        (Value::Int(_), Type::Int) => true,
        (Value::Str(_), Type::Str) => true,
        (Value::Bool(_), Type::Bool) => true,
        (Value::Pair(a, b), Type::Pair(ta, tb)) =>
            matches(a, ta) && matches(b, tb),
        (Value::List(vals), Type::List(t)) =>
            vals.iter().all(|v| matches(v, t)),
        _ => false,
    }
}

use ring::aead;
use ring::rand::{SecureRandom, SystemRandom};

fn key_from_type(ty: &Type) -> aead::LessSafeKey {
    let bytes: [u8; 32] = match ty {
        Type::Int => [0u8; 32],
        Type::Str => [1u8; 32],
        Type::Bool => [2u8; 32],
        Type::Pair(_, _) => [3u8; 32],
        Type::List(_) => [4u8; 32],
    };
    let unbound = aead::UnboundKey::new(&aead::CHACHA20_POLY1305, &bytes).expect("invalid key");
    aead::LessSafeKey::new(unbound)
}

/// Encrypt the given plaintext using the provided `Type` as the key.
pub fn encrypt(ty: &Type, plaintext: &[u8]) -> Vec<u8> {
    let key = key_from_type(ty);
    let rng = SystemRandom::new();
    let mut nonce_bytes = [0u8; 12];
    rng.fill(&mut nonce_bytes).expect("random failure");
    let nonce = aead::Nonce::assume_unique_for_key(nonce_bytes);
    let mut in_out = plaintext.to_vec();
    key.seal_in_place_append_tag(nonce, aead::Aad::empty(), &mut in_out)
        .expect("encryption failure");
    let mut out = nonce_bytes.to_vec();
    out.extend_from_slice(&in_out);
    out
}

/// Decrypt the ciphertext if the value matches the expected type.
pub fn decrypt_with_value(ty: &Type, value: &Value, ciphertext: &[u8]) -> Option<Vec<u8>> {
    if !matches(value, ty) {
        return None;
    }
    if ciphertext.len() < 12 {
        return None;
    }
    let key = key_from_type(ty);
    let mut nonce_bytes = [0u8; 12];
    nonce_bytes.copy_from_slice(&ciphertext[..12]);
    let nonce = aead::Nonce::assume_unique_for_key(nonce_bytes);
    let mut in_out = ciphertext[12..].to_vec();
    let result = key
        .open_in_place(nonce, aead::Aad::empty(), &mut in_out)
        .ok()?;
    Some(result.to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encrypt_decrypt_roundtrip() {
        let ty = Type::Int;
        let value = Value::Int(42);
        let ct = encrypt(&ty, b"hello");
        let pt = decrypt_with_value(&ty, &value, &ct).expect("decrypt");
        assert_eq!(pt, b"hello");
    }

    #[test]
    fn decrypt_rejects_wrong_value() {
        let ty = Type::Int;
        let wrong = Value::Str("oops".into());
        let ct = encrypt(&ty, b"secret");
        assert!(decrypt_with_value(&ty, &wrong, &ct).is_none());
    }

    #[test]
    fn matches_int() {
        assert!(matches(&Value::Int(42), &Type::Int));
        assert!(!matches(&Value::Int(42), &Type::Str));
    }

    #[test]
    fn matches_str() {
        assert!(matches(&Value::Str("hi".into()), &Type::Str));
        assert!(!matches(&Value::Str("hi".into()), &Type::Int));
    }

    #[test]
    fn matches_bool() {
        assert!(matches(&Value::Bool(true), &Type::Bool));
        assert!(!matches(&Value::Bool(true), &Type::Int));
    }

    #[test]
    fn matches_pair() {
        let v = Value::Pair(Box::new(Value::Int(1)), Box::new(Value::Str("a".into())));
        let t = Type::Pair(Box::new(Type::Int), Box::new(Type::Str));
        assert!(matches(&v, &t));
    }

    #[test]
    fn matches_list() {
        let v = Value::List(vec![Value::Int(1), Value::Int(2)]);
        let t = Type::List(Box::new(Type::Int));
        assert!(matches(&v, &t));
    }

    #[test]
    fn multiple_encryptions_use_different_nonces() {
        let ty = Type::Int;
        let value = Value::Int(7);
        let ct1 = encrypt(&ty, b"hello");
        let ct2 = encrypt(&ty, b"hello");
        assert_ne!(ct1, ct2);
        assert_eq!(decrypt_with_value(&ty, &value, &ct1).unwrap(), b"hello");
        assert_eq!(decrypt_with_value(&ty, &value, &ct2).unwrap(), b"hello");
    }
}
