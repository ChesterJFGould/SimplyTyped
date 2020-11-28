# Lexer
This the first stage in the pipeline, the input text is split up into the
individual tokens we care about.
For example `(` becomes `LParen` and `pi` becomes `Identifier pi`.
This is done for two main reasons.
First of all it is much nicer to write a parser for a sequence of tokens than
to write one that directly parses a string.
Secondly it separates the details of the language from the parser, so for
instance if I wanted to add a new sequence of characters that acts like a
`(` (such as `begin` in OCaml) I would just have to add that to the lexer
and not to everything else.
Another more practical example is if I wanted to change which characters could
appear in an Identifier. Right now it is just alphanumeric, but if I wanted to
add unicode support for instance all I would have to change is the lexer.
