use structre::structre;

#[cfg(feature = "unicode")]
#[test]
fn match_() {
    #[structre("(a)(44)")]
    struct Parsed(String, u32);

    let pre = ParsedFromRegex::new();
    let v = pre.parse("a44").unwrap();
    assert_eq!(v.0, "a");
    assert_eq!(v.1, 44);
}
