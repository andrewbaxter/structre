#![cfg(test)]

extern crate structre;

use {
    std::str::FromStr,
    structre::structre,
};

#[test]
fn match_() {
    #[structre("(a)(44)")]
    struct Parsed(String, u32);

    let v = Parsed::from_str("a44").unwrap();
    assert_eq!(v.0, "a");
    assert_eq!(v.1, 44);
}

#[test]
fn named() {
    #[structre("(?P<a>a)(?P<b>44)")]
    struct Parsed {
        a: String,
        b: u32,
    }

    let v = Parsed::from_str("a44").unwrap();
    assert_eq!(v.a, "a");
    assert_eq!(v.b, 44);
}

#[test]
fn uncapture() {
    #[structre("(?:(a))")]
    struct Parsed(String);

    let v = Parsed::from_str("a").unwrap();
    assert_eq!(v.0, "a");
}

#[test]
fn uncapture_named() {
    #[structre("(?:(?P<a>a))")]
    struct Parsed {
        a: String,
    }

    let v = Parsed::from_str("a").unwrap();
    assert_eq!(v.a, "a");
}

#[test]
fn test_struct_opt() {
    #[structre("(?P<a>a)?")]
    struct Parsed {
        a: Option<String>,
    }

    let v = Parsed::from_str("a").unwrap();
    assert_eq!(v.a.as_ref().map(|x| x.as_str()), Some("a"));
}

#[test]
fn test_enum() {
    #[structre("(?P<A>a)|(?P<b>b)")]
    #[derive(PartialEq, Eq, Debug)]
    enum Parsed {
        A(String),
        B {
            b: String,
        },
    }

    let v = Parsed::from_str("a").unwrap();
    assert_eq!(v, Parsed::A("a".to_string()));
}
