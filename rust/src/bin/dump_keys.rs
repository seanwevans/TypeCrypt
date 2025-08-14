use typecrypt::{canonical_bytes, derive_key_bytes, Type};

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

fn main() {
    let types = [
        ("int", Type::Int),
        ("str", Type::Str),
        (
            "pair",
            Type::Pair(Box::new(Type::Int), Box::new(Type::Bool)),
        ),
        ("list", Type::List(Box::new(Type::Int))),
    ];
    for (name, ty) in &types {
        let mut bytes = Vec::new();
        canonical_bytes(ty, &mut bytes);
        let key = derive_key_bytes(ty);
        println!(
            "{{\"type\":\"{}\",\"bytes\":{:?},\"key\":\"{}\"}}",
            name,
            bytes,
            hex(&key)
        );
    }
}
