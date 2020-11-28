# Description
An implementation of the simply typed lambda calculus
written mostly for learning purposes.
Running reduce.sh on a file containing a lambda expression will print the
beta-reduced equivalent.

# Dependencies
+ [OCaml](https://ocaml.org/)
+ [Dune](https://dune.build/)

# Installation
1. Clone this repository
2. Run `dune build`
3. Running `./reduce.sh example.src` should print `(s : String)`

# Syntax
The [simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
is essentially the regular lambda calculus but all variables are associated
with a type.
As the name would suggest the types are extremely simple, they can be either a
base type such as `string` or a function type such as `string -> int` which
takes in an expression with type `string` and produces and expression with
type `int`.
For example the string identity function would be written as `\s : string. s`
and would be applied like `(\s : string. s) (greeting : string)`.
As you can see to facilitate writting lambda expressions we use `\` instead of
`λ`(the actual lambda symbol).

# Why?
I'm really interested in language design and this kind of project seemed like a
good way to gain experience in implementating programming languages as it's
quite a small project so any mistakes I make won't seriously cost me any time.
This project especially was meant to help me learn about implementing type
systems.

# Why OCaml?
1. Pattern matching is awesome for this kind of thing
2. Go doesn't have good support for union types (if it did I would probably be
using that as it's subjectively the best programming language yet)
3. https://www.cs.ubc.ca/~murphyk/Software/Ocaml/why_ocaml.html
