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

Define a structure and use this macro to implement `TryFrom` (and `FromStr` if the type has no lifetimes):

```
#[structre("(?P<key>[^:]+): (?P<value>\\d+)")]
struct KV {
    key: String,
    value: usize,
}
```

```
let m = KV::try_from("hi: 39393")?;
```

Both `try_from` and `from_str` returns a result with error type `structre::Error`. The `structre::Error::Field` result only occurs if a field's `try_from` or `from_str` method fails - if all of your fields are strings, you can only get `structre::Error::NoMatch`.

# Expressing regexes with types

- Alternate (`|`) captures can be parsed as enums

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

- Non-alternate captures:

  All captures must correspond to a field. Named captures correspond to named fields, unnamed captures correspond to unnamed fields (ex: tuple elements). Repetitions, `?`, and `|` will make a capture optional so the corresponding field must also be optional.

The following types are supported for fields:

- Simple types: any type implementing `std::convert::TryFrom<&str>`

  This includes `&str` if you want non-allocating parsing.

- Simple types: Standard library/core types that implement `std::str::FromStr`

  The standard library doesn't implement `TryFrom<&str>` for any core types currently so an internal database is used to (roughly) identify that a field has a core type and switches to `FromStr` for that.

- Options with a simple type inside

- Tuples with either options (as above) or simple types inside

See the [./crates/structre/tests/tests.rs](tests) for some simple examples.

# Limitations

I was hoping to be able to ensure that the regex has valid characters for numbers, but due to the above and the difficulty of reasoning about the contents of regex ASTs I had to scrap that.

Non-unicode parsing isn't currently supported. One issue is I couldn't find an ascii float parsing library. If this is important and you have a vision of how it could work please raise an issue!

The regex is lazily compiled and stored statically. Originally I made the regex compilation manual and explicit, but this made the ergonomics much worse (managing parsers) and prevented things like implementing `FromStr`. In `0.1.0` I changed it to statically instantiate the regex. I'd be open to making this configurable in the future, either having an option to manually manage the compiled regex or else compiling on every parse for rarely used regexes.

~~String references and other reference types~~ Reference types are now supported via `TryFrom<&T>` starting in `0.2.0`! There was a large discussion
at <https://www.reddit.com/r/rust/comments/1h2f6lt/structre_staticchecked_parsing_of_regexes_into/>. In the end I decided to go with basing all use around `TryFrom` instead of `FromStr` with special cases:

- Both approaches have special cases: The former has a database of standard library/core types that don't support `TryFrom` to switch to `FromStr`, the latter has carveouts for `&str` and possibly other types (`Cow`?)
- I think the code for identifing special cases in the latter is more difficult; in the former, all the types are non-generic non-reference types, most without `::` paths
- Hopefully `TryFrom` support will grow, and at some point the carveouts won't be needed - it seems to be the future-facing choice
- `TryFrom` should allow users to wrap more types than `FromStr`, without needing annotations to explicitly switch the parsing method/trait
