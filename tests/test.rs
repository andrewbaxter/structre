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

#[cfg(feature = "unicode")]
#[test]
fn named() {
    #[structre("(?P<a>a)(?P<b>44)")]
    struct Parsed {
        a: String,
        b: u32,
    }

    let pre = ParsedFromRegex::new();
    let v = pre.parse("a44").unwrap();
    assert_eq!(v.a, "a");
    assert_eq!(v.b, 44);
}

#[cfg(feature = "unicode")]
#[test]
fn uncapture() {
    #[structre("(?:(a))")]
    struct Parsed(String);

    let pre = ParsedFromRegex::new();
    let v = pre.parse("a").unwrap();
    assert_eq!(v.0, "a");
}

#[cfg(feature = "unicode")]
#[test]
fn uncapture_named() {
    #[structre("(?:(?P<a>a))")]
    struct Parsed {
        a: String,
    }

    let pre = ParsedFromRegex::new();
    let v = pre.parse("a").unwrap();
    assert_eq!(v.a, "a");
}
