use {
    std::{
        fmt::Display,
    },
};
pub use {
    structre_proc_macros::structre,
    regex,
};

#[derive(Debug)]
pub enum Error {
    NoMatch,
    Field {
        field: &'static str,
        error: String,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NoMatch => {
                format_args!("No match").fmt(f)
            },
            Error::Field { field, error } => {
                format_args!("Error parsing field {}: {}", field, error).fmt(f)
            },
        }
    }
}

impl std::error::Error for Error { }
