use ring::hkdf;
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
    let salt = hkdf::Salt::new(hkdf::HKDF_SHA256, b"TypeCryptHKDFSalt");
    let prk = salt.extract(&bytes);
    let okm = prk
        .expand(&[b"TypeCryptHKDFInfo"], hkdf::HKDF_SHA256)
        .expect("hkdf expand");
    let mut out = [0u8; 32];
    okm.fill(&mut out).expect("hkdf fill");
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
