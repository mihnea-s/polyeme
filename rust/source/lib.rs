pub fn hello() -> &'static str {
  "Hello, Polyeme!"
}

#[test]
fn test_hello() -> Result<(), &'static str> {
  match hello() {
    "Hello, Polyeme!" => Ok(()),
    _ => Err("expected a hello"),
  }
}
