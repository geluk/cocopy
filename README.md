A compiler for ChocoPy.

My main goal is to (nearly) fully implement ChocoPy as defined in the language
reference. In addition, I'd like to experiment with some additional features,which are described below.

# TODO

## Lexical structure:

- [x] Token lexing
  - [x] Keywords
  - [x] Symbols
  - [x] Structure
    - [x] Newline
    - [x] Tab-based indentation
    - [ ] Space-based indentation (postponed for now)
  - [x] Identifiers
  - [x] Integer literals
  - [x] String literals
  - [x] Comments
- [x] Structural normalisation (emitting final newlines and dedents)
- [x] Source code positions

## Syntax:

- [x] Literals
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
- [ ] Blocks
- [x] Contextual error reporting

## Type checking:

- [x] Literals
- [x] Variables
  - [x] Initialisation
- [ ] Statements
  - [x] Expression evaluation
  - [x] Assignment
  - [ ] `if`
  - [ ] `while`
  - [ ] `for`
  - [x] `pass`
  - [ ] `return`
- [ ] Expressions
  - [x] Unary
    - [x] Negate
    - [x] Not
  - [x] Binary
    - [x] Arithmetic
    - [x] Integer comparison
    - [x] Boolean comparison
    - [x] Boolean combination
    - [ ] String operations
    - [ ] `is`
  - [x] Ternary
  - [ ] Object construction
  - [ ] List display
  - [ ] List operators
  - [ ] Attribute access
  - [ ] Multiple assignment
  - [ ] Function application
- [ ] Function definitions
- [ ] Class definitions
- [ ] Global environment

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
