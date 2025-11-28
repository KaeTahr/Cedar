# Cedar: A Data Layout Language for C

This repository contains the prototype implementation of **Cedar**, a data layout description language for C.
Cedar allows programmers to specify low-level record layouts declaratively, with explicit byte offsets, relative positioning, and endianness annotations.
It follows the design of Dargent (Cogent) and generates C accessors for structured memory layouts.

---

## Overview

The Cedar compiler (`cedarc`) reads `.cedar` layout files and produces C header (`.h`) and implementation (`.c`) files containing field-level getters and setters.

Example:

```cedar
layout Student = record {
  grade : record {
    maths   : 1B @ 0B,
    physics : 2B after maths
  } @ 0B,
  name : 1B after grade,
  age  : 1B before name
} @ 0B;
```

produces

```
out/student.h
out/student.c
```

The generated C code defines:

```c
struct Student { unsigned int data[N]; };
```

and functions such as:

```c
uint32_t d_get_name(const Student *b);
void d_set_name(Student *b, uint32_t v);
```

## Building the Compiler

Requirements:

- GHC 9.6+ (Glasgow Haskell Compiler)
- Cabal 3.1+
- Standard system tools (gcc, make)

Build everything with:

```sh
cabal build
```

This produces two exectuables:

- cedar-demo -- OLD demo with a hardcoded demonstration
- cedarc -- current Cedar compiler for .cedar files

## Running the Compiler

Compile a .cedar layout into C:
```sh
cabal run cedarc -- examples/student.cedar Student out
```

This command:

- Parses example/student.cedar
- Looks for a layout named Student
- Emits out/student.hs and out/student.c
You can inspect or compile these files using a
C99 compiler.

Generated code was tested during development using

```sh
cd out
gcc -Wall -O2 -std=c99 -o student_test main.c student.c
```

## Implemented Features

- Nested records
- Absolute offsets (@NB)
- Relative offsets (after/before)
- Optional distance (after name 2B)
- Per-field endianness (endian LE | BE | ME)
- Quoted field names ("Valid name")
- Uniform fixed-length arrays (1B[10])

## Known Limitations

- Unions are not yet supported
- Sub-type fields are not yet supported.
- No arithmetic expressions inside offsets.
- Commends are not lexed and will cause errors
- Single-letter identifiers such as B may collide with the unit token
- Generated C code flattens all records into a single data array
- Pointers are not yet supported

## Directory Structure

```
app/                     Main entry points (cedarc, cedar-demo)
src/Cedar/               Compiler implementation
src/Cedar/Frontend/      Parser and lexer (Alex/Happy)
src/Cedar/Semantic/      Intermediate representations (C, L, LR)
src/Cedar/CodeGen/       C code generation
examples/                Example Cedar layout files
out/                     Generated output
```

## Summary of Commands

```sh
# Build compiler
cabal build

# Generate C for a layout
cabal run cedarc -- examples/student.cedar Student out

# Compile generated code
cd out
gcc -Wall -O2 -std=c99 -o student_test main.c student.c
./student_test
```
