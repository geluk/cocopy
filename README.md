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

# Overview

Currently, the compiler is capable of compiling very basic ChocoPy expressions
to assembly. The following steps are taken during compilation.

## ChocoPy code
Start here.
```py
a:int = 0
b:int = 9
a = b + 10 * 3
b = a * 100
```
## After lexing
The lexer emits a token stream indicating the type of each token (keyword,
identifier, symbol, literal, etc..) and its value, if present.
```
"a" ":" "int" "=" "0" "\n" "b" ":" "int" "=" "9" "\n" "a" "=" "b" "+" "10" "*" "3" "\n" "b" "=" "a" "*" "100" "\n"
```
## After parsing and type checking
The parser combines tokens into expressions and statements, which together
form an abstract syntax tree. The type checker verifies that all operations
evaluate to the correct types.

The syntax tree can be pretty-printed like so:
```py
a : int = 0
b : int = 9
a = (b + (10 * 3))
b = (a * 100)
```
## Intermediate code generation
The IL generator converts the syntax tree into three-address code.
Complex expressions are flattened, generating lots of intermediates (`%`).
To prevent variable reuse, variables receive a new subscript (`^`)
every time they're reassigned. This will make it easier for the optimiser to
reason about program flow.
```
a^1 = 0
b^1 = 9
%t1 = 10 * 3
%t2 = b^1 + %t1
a^2 = %t2
%t3 = a^2 * 100
b^2 = %t3
```
## Intermediate code optimisation
The optimiser finds statements that can be removed or simplified, and rewrites
the intermediate code.
```
%t1 = 10 * 3
%t2 = 9 + %t1
%t3 = %t2 * 100
```
## Assembly generation
Each instruction is converted to assembly. Operations ma be broken up
further into a `store` and `apply` instruction when they cannot be converted
to a single assembly instruction. The register allocator assigns registers to
variables, keeping track of which ones are used.
```asm
main:
    mov     rax, 10             ; <store> %t1 = 10 * 3
    imul    rax, 3              ; <apply> %t1 = 10 * 3
    mov     rbx, 9              ; <store> %t2 = 9 + %t1
    add     rbx, rax            ; <apply> %t2 = 9 + %t1
    mov     rcx, rbx            ; <store> %t3 = %t2 * 100
    imul    rcx, 100            ; <apply> %t3 = %t2 * 100
```

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

- [x] Remove unused assignments
- [x] Inline assigned variables when posssible

## Target code generation

- [x] Proof-of-concept
  - [x] Generate assembly code
  - [x] Assemble object files with `nasm`
  - [x] Linking with `gcc` or `link.exe`
- [ ] Convert intermediate code to assembly
  - [x] Integer arithmetic
  - [x] Procedure calls
  - [ ] Boolean operations
  - [ ] Array lookup
  - [ ] Pointer dereference
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