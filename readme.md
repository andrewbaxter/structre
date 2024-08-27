Statically-checked regex parsing into structs.

This avoids common regex pitfalls like

- Off by one capture indexes
- Trying to get nonexistent captures
- Desync of capture names in regex and the names used to fetch fields

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

The `structre::Error::Field` result only occurs if a field's `from_str` method fails - if all of your fields are strings, you can only get `Error::NoMatch`.

# Supported structures

Structs and enums both work, although there are some slight nuances.

- In structures:

  All captures must correspond to a field. Named captures correspond to named fields, unnamed captures correspond to unnamed fields (ex: tuple elements). Repetitions, `?`, and `|` will make a capture optional, and the corresponding field must also be optional.

- In enums:

  All variants must either

  - Have at least one non-optional named field where the name matches a named capture

  - Be a 1-tuple with a non-optional type. In this case, the variant name must match a named capture

  The presence of the matching named capture in the result will be used to determine which variant was parsed.

Generally speaking the following types are suppored:

- Simple types: any type implementing `std::str::FromStr`

- Options with a simple type inside

- Tuples with either options (as above) or simple types inside

See the [./crates/structre/tests/tests.rs](tests) for some simple examples.

# Limitations

I was hoping to be able to ensure that the regex has valid characters for numbers, but due to the above and the difficulty of reasoning about the contents of regex ASTs I had to scrap that.

Non-unicode parsing isn't currently supported. I couldn't find an ascii float parsing library. If this is important and you have a vision of how it could work please raise an issue!

# Design

The main goals are, in order of importance:

1. Safety

2. Ease of use

3. Performance

Originally I made the regex compilation manual and explicit, but this made the ergonomics much worse (managing parsers) and prevented things like implementing `FromStr`. In `0.1.0` I changed it to statically instantiate the regex. I'd be open to making this configurable in the future, either having an option to manually manage the compiled regex or else compiling on every parse for rarely used regexes.
