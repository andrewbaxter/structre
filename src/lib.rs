pub use anyhow::{Error, Result};
pub use once_cell::sync::Lazy;
pub use structre_proc_macros::structre;

#[cfg(feature = "bytes")]
pub mod structre_bytes {
    use atoi::{FromRadix10, FromRadix10Signed};
    pub use regex::bytes::Regex as BytesRegex;

    trait FromU8Str: Sized {
        fn from_str(str: &[u8]) -> Option<Self>;
    }

    impl FromU8Str for Box<[u8]> {
        fn from_str(str: &[u8]) -> Option<Self> {
            Box::new(str)
        }
    }

    impl<T: FromRadix10> FromU8Str for T {
        fn from_str(str: &[u8]) -> Option<Self> {
            atoi::atoi(str)
        }
    }

    impl<T: FromRadix10Signed> FromU8Str for T {
        fn from_str(str: &[u8]) -> Option<Self> {
            atoi::atoi(str)
        }
    }
}
#[cfg(feature = "bytes")]
pub use structre_bytes::*;

#[cfg(feature = "unicode")]
pub use regex::Regex as UnicodeRegex;
