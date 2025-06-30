//! TypeCrypt Rust (Production Branch)
//!
//! This crate will implement the hardened TypeCrypt engine.

#[derive(Debug)]
pub enum Type {
    Int,
    Str,
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Str(String),
}
