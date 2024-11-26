<table align="right" margin="1em"><tr>
<td><a href="https://crates.io/crates/structre"><img alt="crates.io" src="https://img.shields.io/crates/v/structre"></a></td>
<td><a href="https://docs.rs/structre"><img alt="docs.rs" src="https://img.shields.io/docsrs/structre"></td></a>
</tr></table>

Statically-checked regex parsing into structs/enums.

This avoids common regex pitfalls like

- Off by one capture indexes
- Trying to get nonexistent captures
- Desync of capture names in regex and the names used to fetch fields

Note: This isn't like serde in that it doesn't work on arbitrary structs/enums. The struct/enum definition must be written to match the regex.

# Installation

```sh
cargo add structre
```

# Use

Define a structure and use this macro to generate a parser helper struct:

```
#[structre("(?P<key>[^:]+): (?P<value>\\d+)")]
struct KV {
    key: String,
    value: usize,
}
```

```
let m = KV::from_str("hi: 39393")?;
```

`from_str` returns a result with error type `structre::Error`. The `structre::Error::Field` result only occurs if a field's `from_str` method fails - if all of your fields are strings, you can only get `structre::Error::NoMatch`.

# Supported structures

Structs and enums both work, although there are some slight nuances.

- In structures:

  All captures must correspond to a field. Named captures correspond to named fields, unnamed captures correspond to unnamed fields (ex: tuple elements). Repetitions, `?`, and `|` will make a capture optional so the corresponding field must also be optional.

- In enums:

  All variants must either

  - Have at least one non-optional uniquely-named field where the name matches a named capture:

    Ex:

    ```rust
    #[structre("(?<a_field>.*)|(?<b_field>.*)")]
    enum AOrB {
        A {
            a_field: String,
        },
        B {
            b_field: String,
        }
    }
    ```

    The enum variant is determined by the presence of either the `a_field` capture or `b_field` capture.

  - Be a 1-tuple with a non-optional type. In this case, the variant name must match a named capture:

    Ex:

    ```rust
    #[structre("(?<A>.*)|(?<B>.*)")]
    enum AOrB {
        A(String),
        B(String),
    }
    ```

    The enum variant is determined by the presence of either the `A` capture or `B` capture.

The following types are supported for fields:

- Simple types: any type implementing `std::str::FromStr`

- Options with a simple type inside

- Tuples with either options (as above) or simple types inside

See the [./crates/structre/tests/tests.rs](tests) for some simple examples.

# Limitations

I was hoping to be able to ensure that the regex has valid characters for numbers, but due to the above and the difficulty of reasoning about the contents of regex ASTs I had to scrap that.

Non-unicode parsing isn't currently supported. One issue is I couldn't find an ascii float parsing library. If this is important and you have a vision of how it could work please raise an issue!

The regex is lazily compiled and stored statically. Originally I made the regex compilation manual and explicit, but this made the ergonomics much worse (managing parsers) and prevented things like implementing `FromStr`. In `0.1.0` I changed it to statically instantiate the regex. I'd be open to making this configurable in the future, either having an option to manually manage the compiled regex or else compiling on every parse for rarely used regexes.
