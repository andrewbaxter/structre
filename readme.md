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

The parser is named `KVFromRegex`.

Instantiate the parser and use it like:

```
let kv_parser = KVFromRegex::new();
let m: KV = kv_parser.parse("hi: 39393")?;
```

`parse` returns a Result. An `Err` is only returned in the case that there's no match or one of the field's `from_str` returns an `Err`. Per the preceding, if you only have `String` fields an `Err` always means no match, so you can treat it as a `None`.

# Supported types

The parsed data can be a structure with named fields or a tuple.

If it has named fields:

- Every field must have an associated named capture
- Every named capture must have an associated field
- There must be no unnamed captures
- Nested structures and tuples within structures aren't supported since there's no clear correct way to handle it.

If it's a tuple:

- The number of captures must equal the number of tuple elements
- There must be no named captures
- Nested tuples are okay

Any field that implements `std::str::FromStr` will work.

Note: the static analysis stops at any non-inline type (ex: `struct MyData(SomeOtherData);` - so if `SomeOtherData` is a tuple or has fields, these will not be checked and `SomeOtherData` will be parsed with `FromStr` from a single capture).

# Limitations

As noted above, not much type available so only the structure above can be checked. Externally defined types will not be inspected.

I was hoping to be able to ensure that the regex has valid characters for numbers, but due to the above and the difficulty of reasoning about the contents of regex ASTs I had to scrap that.

Non-unicode parsing isn't currently supported. I couldn't find an ascii float parsing library and this isn't maximally optimized in the first place due to the use of `anyhow`. If this is important and you have a vision of how it could work please raise an issue!
