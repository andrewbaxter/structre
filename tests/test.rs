#[cfg(feature = "unicode")]
#[test]
fn match_() {
    use std::str::FromStr;

    use structre::structre;
    #[structre("(a)(44)")]
    struct Parsed(String, u32);
    let v = Parsed::from_str("a44").unwrap();
    assert_eq!(v.0, "a");
    assert_eq!(v.1, 44);
}
