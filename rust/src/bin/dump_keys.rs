use ring::digest;
use typecrypt::Type;

fn canonical_bytes(ty: &Type, out: &mut Vec<u8>) {
    match ty {
        Type::Int => out.push(0),
        Type::Str => out.push(1),
        Type::Bool => out.push(2),
        Type::Pair(a, b) => {
            out.push(3);
            canonical_bytes(a, out);
            canonical_bytes(b, out);
        }
        Type::List(t) => {
            out.push(4);
            canonical_bytes(t, out);
        }
    }
}

fn derive_key_bytes(ty: &Type) -> [u8; 32] {
    let mut bytes = Vec::new();
    canonical_bytes(ty, &mut bytes);
    let hash = digest::digest(&digest::SHA256, &bytes);
    let mut out = [0u8; 32];
    out.copy_from_slice(hash.as_ref());
    out
}

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
