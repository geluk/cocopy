A compiler for ChocoPy.

My main goal is to (nearly) fully implement ChocoPy as defined in the language
reference. In addition, I'd like to experiment with some additional features,which are described below.

# Running the compiler

Build the compiler first:
```
cargo build --release
```

Specify the source file as the first argument:

```
./target/release/cocopy ./examples/simple_add_program.py
```

This generates an `out.exe` file on Windows, assuming you have a Visual Studio 2022 installation
with `link.exe`. Otherwise, you may need tweak and run `build_win64.ps1` instead, which will
manually link `out.obj` into an executable.

On Linux, you must link `out.obj` into an executable yourself. You can use `gcc` for this.


# General structure


# TODO

## Lexical structure

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

## Syntax

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

## Type checking

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
- [ ] Overload resolution

## Intermediate code generation

- [x] Literals
- [ ] Statements
  - [x] Expression evaluation
  - [x] Assignment
  - [ ] `if`
  - [ ] `while`
  - [ ] `for`
  - [x] `pass`
  - [ ] `return`
- Expressions
  - [x] Literal
  - [x] Identifier
  - [x] Binary

## Optimisation passes

- [ ] Everything

## Target code generation

- [x] Proof-of-concept
  - [x] Generate assembly code
  - [x] Assemble object files with `nasm`
  - [x] Linking with `gcc` or `link.exe`
- [ ] Convert intermediate code to assembly
  - [x] Integer arithmetic
  - [ ] Everything else
- [ ] Register allocation
  - [x] Naive register allocation
  - [ ] Store on stack when registers are exhausted
  - [ ] Reuse registers after final reference
  - [ ] Free registers when required by a different operation

## Operational semantics:

- [ ] Everything

# Additional goals

- Multiple backends (native, LLVM)
- Using Hindley-Milner type inference to make all type annotations optional
- Good error reporting
- More features?

# Assembly cheat sheet

| Register | Usage                                                        |
| -------- | ------------------------------------------------------------ |
| `rbp`    | Frame pointer, points to the base of the current stack frame |
| `rsp`    | Stack pointer, points to the top of the current stack frame  |


# Additional reading

## ChocoPy
- [ChocoPy language reference](./chocopy_language_reference.pdf)
- [The ChocoPy website](https://chocopy.org/)

## Type checking
- [Write You a Haskell](http://dev.stephendiehl.com/fun/)

## Code generation
- [My First Language Frontend with LLVM](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)
- [x64 software conventions (Windows)](https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions?view=msvc-170)
- [What Every Programmer Should Know About Memory](https://people.freebsd.org/~lstewart/articles/cpumemory.pdf)
- [Understanding Windows x64 Assembly](https://sonictk.github.io/asm_tutorial/)
- [x86 instruction reference](https://www.felixcloutier.com/x86/)