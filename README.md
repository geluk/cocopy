A compiler for ChocoPy.

My main goal is to (nearly) fully implement ChocoPy as defined in the language
reference. In addition, I'd like to experiment with some additional features,
which are described below.

# Running the compiler

Build the compiler first:
```
cargo build --release
```

Run `target/release/cocopy.exe`:
```
cocopy
A compiler for ChocoPy

USAGE:
    cocopy.exe [OPTIONS] <SUBCOMMAND>

OPTIONS:
    -h, --help                 Print help information
    -v, --verbose <VERBOSE>    [default: 1]

SUBCOMMANDS:
    check      Check a program for errors
    compile    Compile a program
    help       Print this message or the help of the given subcommand(s)
    run        Compile and run
```

To check, compile, or run a file, specify it as the first argument:

```
cocopy run ./examples/simple_add_program.py
```

The compilation step generates an `out/out.exe` file on Windows, assuming you
have a Visual Studio 2022 installation with the C++ workload installed.
Otherwise, you may need tweak and run `build_win64.ps1` instead, which will
manually link `out.obj` into an executable.

On Linux, the compiled executable is written to `out/out`. Note that you must
have `nasm` and `gcc` installed. As with Windows, you can also use the
`build_linux64.sh` script to assemble and link the executable.

# Overview

Currently, the compiler is capable of compiling basic ChocoPy programs
to assembly. The following steps are taken during compilation.

## ChocoPy code
Start here.
```py
a:int = 0
b:int = 9
a = b + 10 * 3
b = a * 100
if b > 10:
    print(b)
```
## After lexing
The lexer emits a token stream indicating the type of each token (keyword,
identifier, symbol, literal, etc..) and its value, if present. It also detects
indentation changes, emitting special `indent` and `dedent` tokens whenever
the indentation level changes.
```
"a" ":" "int" "=" "0" "\n"
"b" ":" "int" "=" "9" "\n"
"a" "=" "b" "+" "10" "*" "3" "\n"
"b" "=" "a" "*" "100" "\n"
"if" "b" ">" "10" ":" "\n"
<indent>
"print" "(" "b" ")" "\n"
<dedent>
```
## After parsing and type checking
The parser combines tokens into expressions and statements, which together
form an abstract syntax tree. The type checker verifies that all operations
evaluate to the correct types.

The syntax tree is represented like so:
```yaml
program:
  var_defs:
  - name: a
    type_spec: int
    value: integer(0)
  - name: b
    type_spec: int
    value: integer(9)
  statements:
  - assign:
      target: identifier(a)
      value:
        binary:
          lhs: identifier(b)
          op: add
          rhs:
            binary:
              lhs:
                literal: integer(10)
              op: multiply
              rhs:
                literal: integer(3)
  - assign:
      target: identifier(b)
      value:
        binary:
          lhs: identifier(a)
          op: multiply
          rhs:
            literal: integer(100)
  - if:
      condition:
        binary:
          lhs: identifier(b)
          op: greater_than
          rhs:
            literal: integer(10)
      body:
        statements:
        - evaluate:
            expression:
              function_call:
                name: print
                args:
                - identifier(b)
```
## Intermediate code generation
The IL generator converts the syntax tree into three-address code.
Complex expressions are flattened, generating lots of intermediates (`%`).
To prevent variable reuse, variables receive a new subscript (`^`)
every time they're reassigned. This will make it easier for the optimiser to
reason about program flow.
```
function main
        a^1 = 0
        b^1 = 9
        %1 = 10 * 3
        %2 = b^1 + %1
        a^2 = %2
        %3 = a^2 * 100
        b^2 = %3
        if b^2 <= 10 goto if_end_1
        %4 = call print (b^2)
    if_end_1:
```
## Intermediate code optimisation
The optimiser finds statements that can be removed or simplified, and rewrites
the intermediate code.
```
function main
        %1 = 10 * 3
        %2 = 9 + %1
        %3 = %2 * 100
        if %3 <= 10 goto if_end_1
        call print (%3)
    if_end_1:
```
## Assembly generation
Each instruction is converted to assembly. Operations may be broken up
further into a `store` and `apply` instruction when they cannot be converted
to a single assembly instruction. The register allocator assigns registers to
variables, keeping track of where registers are used, and deallocating them
as soon as their value is no longer needed.
```asm
main:
    push    rbp                 ; store base pointer

                                ; %1 = 10 * 3
    mov     rax, 10
    imul    rax, 3
                                ; %2 = 9 + %1
    mov     rbx, 9
    add     rbx, rax
                                ; %3 = %2 * 100
    mov     rcx, rbx
    imul    rcx, 100
                                ; if %3 <= 10 goto if_end_1
    mov     rax, 10
    cmp     rcx, rax
    jle     .if_end_1
                                ; call print (%3)
    push    rcx
    sub     rsp, 8
    call    print
    add     rsp, 8
    pop     rcx
  .if_end_1:
    xor     rax, rax            ; return 0
    pop     rbp                 ; restore previous base pointer
    ret                         ; return to caller
```

# TODO

## Lexical structure

- [x] Token lexing
  - [x] Keywords
  - [x] Symbols
  - [x] Structure
    - [x] Newline
    - [x] Tab-based indentation
    - [x] Space-based indentation
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
- [x] Functions
  - [x] Function definition
  - [x] Function body
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
  - [x] Function/method call
  - [x] Operator precedence and associativity
  - [ ] Handle assignment targets differently from binary expressions
- [ ] Statements
  - [x] Expression evaluation
  - [x] Assignment
  - [ ] `if`
    - [x] Body
    - [x] `else`
    - [ ] `elif`
  - [x] `while`
  - [ ] `for`
  - [x] `pass`
  - [x] `return`
- [x] Blocks
- [x] Contextual error reporting
- [ ] Split assignment target and expression
  - [ ] Create AST nodes
  - [ ] Create assignment target parsing functions

## Type checking

- [x] Literals
- [x] Variables
  - [x] Initialisation
- [ ] Statements
  - [x] Expression evaluation
  - [x] Assignment
  - [ ] `if`
    - [x] Body
    - [x] `else`
    - [ ] `elif`
  - [x] `while`
  - [ ] `for`
  - [x] `pass`
  - [x] `return`
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
  - [x] Function application
  - [ ] Method call
  - [ ] Object construction
  - [ ] List display
  - [ ] List operators
  - [ ] Attribute access
  - [ ] Multiple assignment
  - [x] Function application
- [x] Function definitions
- [ ] Class definitions
- [ ] Global environment
- [x] Overload resolution

## Intermediate code generation

- [x] Literals
- [ ] Statements
  - [x] Expression evaluation
  - [x] Assignment
  - [ ] `if`
    - [x] Special-case comparison statements
    - [x] Body
    - [x] `else`
    - [ ] `elif`
  - [x] `while`
  - [ ] `for`
  - [x] `pass`
  - [x] `return`
- Expressions
  - [x] Literal
  - [x] Identifier
  - [x] Binary
  - [x] Procedure call
  - [ ] Object construction
  - [ ] List display
  - [ ] List operators
  - [ ] Attribute access
  - [ ] Multiple assignment
- [x] Function definitions
- [ ] Class definitions

## Optimisation passes

- [x] Remove unused assignments
- [x] Inline assigned variables when posssible
- [x] Remove φ-functions
- [ ] When unifying two names, prefer variables over temporaries

## Target code generation

- [x] Proof-of-concept
  - [x] Generate assembly code
  - [x] Assemble object files with `nasm`
  - [x] Linking with `gcc` or `link.exe`
- [ ] Convert intermediate code to assembly
  - [x] Integer arithmetic
  - [x] Procedure calls
  - [x] Procedure return
  - [x] Boolean operations
  - [x] Goto
  - [x] Conditional goto
  - [ ] Array lookup
  - [ ] Pointer dereference
  - [ ] Everything else
- [ ] Register allocation
  - [x] Naive register allocation
  - [ ] Spill to stack when registers are exhausted
  - [x] Reuse registers after final reference
  - [x] Free registers when required by a different operation
  - [x] Save and restore registers between function calls

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
- [Algorithm 𝒲 Step by Step](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf)
- [Generalizing Hindley-Milner Type Inference Algorithms](http://www.cs.uu.nl/research/techreps/repo/CS-2002/2002-031.pdf)

## Code generation
- [My First Language Frontend with LLVM](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)
- [Lecture Notes on Static Single Assignment Form](https://www.cs.cmu.edu/~rjsimmon/15411-f15/lec/10-ssa.pdf)
- [x64 software conventions (Windows)](https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions?view=msvc-170)
- [What Every Computer Scientist Should Know About Floating-Point Arithmetic](https://www.itu.dk/~sestoft/bachelor/IEEE754_article.pdf)
- [What Every Programmer Should Know About Memory](https://people.freebsd.org/~lstewart/articles/cpumemory.pdf)
- [Understanding Windows x64 Assembly](https://sonictk.github.io/asm_tutorial/)
- [x86 instruction reference](https://www.felixcloutier.com/x86/)
- [Register Allocation (via graph coloring)](http://web.cecs.pdx.edu/~mperkows/temp/register-allocation.pdf)