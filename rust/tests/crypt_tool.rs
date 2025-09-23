use assert_cmd::prelude::*;
use std::process::Command;

const PLAINTEXT: &str = "cross-test\n";

fn run_encrypt(ty: &str) -> Result<String, Box<dyn std::error::Error>> {
    let output = Command::cargo_bin("crypt_tool")?
        .args(["encrypt", ty])
        .output()?;
    assert!(output.status.success(), "encryption failed: {output:?}");
    let stdout = String::from_utf8(output.stdout)?;
    Ok(stdout.trim().to_string())
}

fn run_decrypt(ty: &str, ct: &str) -> Result<String, Box<dyn std::error::Error>> {
    let output = Command::cargo_bin("crypt_tool")?
        .args(["decrypt", ty, ct])
        .output()?;
    assert!(output.status.success(), "decryption failed: {output:?}");
    Ok(String::from_utf8(output.stdout)?)
}

#[test]
fn encrypt_decrypt_bool_roundtrip() -> Result<(), Box<dyn std::error::Error>> {
    let ciphertext = run_encrypt("bool")?;
    let plaintext = run_decrypt("bool", &ciphertext)?;
    assert_eq!(plaintext, PLAINTEXT);
    Ok(())
}

#[test]
fn encrypt_decrypt_list_of_ints_roundtrip() -> Result<(), Box<dyn std::error::Error>> {
    let ciphertext = run_encrypt("list<int>")?;
    let plaintext = run_decrypt("list<int>", &ciphertext)?;
    assert_eq!(plaintext, PLAINTEXT);
    Ok(())
}

#[test]
fn encrypt_decrypt_default_list_roundtrip() -> Result<(), Box<dyn std::error::Error>> {
    let ciphertext = run_encrypt("list")?;
    let plaintext = run_decrypt("list", &ciphertext)?;
    assert_eq!(plaintext, PLAINTEXT);
    Ok(())
}
