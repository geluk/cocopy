A compiler for ChocoPy.

My main goal is to (nearly) fully implement ChocoPy as defined in the language
reference. In addition, I'd like to experiment with some additional features,which are described below.

# TODO

## Lexical structure:

- [ ] Token lexing
  - [x] Keywords
  - [x] Symbols
  - [x] Structure
    - [x] Newline
    - [x] Tab-based indentation
    - [ ] Space-based indentation (postponed for now)
  - [x] Identifiers
  - [x] Integer literals
  - [ ] String literals
  - [ ] Comments
- [x] Structural normalisation (emitting final newlines and dedents)
- [x] Source code positions

## Syntax:

- [x] Variables
  - [x] Type annotations
- [ ] Functions
  - [ ] Function body
  - [ ] `global`
  - [ ] `nonlocal`
- [ ] Classes
  - [ ] Class body
- [x] Expressions
  - [x] Literal
  - [x] Identifier
  - [x] Unary
  - [x] Binary
  - [x] Ternary
  - [x] Operator precedence and associativity
  - [ ] Handle assignment targets differently from binary expressions
- [ ] Statements
  - [x] Expression evaluation
  - [x] Assignment
  - [ ] `if`
  - [ ] `while`
  - [ ] `for`
  - [x] `pass`
  - [x] `return`
- [x] Literals
- [ ] Blocks
- [x] Contextual error reporting

## Type rules:

- [ ] Everything

## Operational semantics:

- [ ] Everything

# Additional goals

- Multiple backends (native, LLVM)
- Using Hindley-Milner type inference to make all type annotations optional
- Good error reporting
- More features?

# Additional reading

- [ChocoPy language reference](./chocopy_language_reference.pdf)
- [The ChocoPy website](https://chocopy.org/)
- [My First Language Frontend with LLVM](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)
- [Write You a Haskell](http://dev.stephendiehl.com/fun/)
