# CompilerAndInterpreter

This is an educational project that implements a small functional programming language with both a compiler and an interpreter written in F#.

## Features

- **Lexical Analysis** – Tokenizes the input source code.
- **Parsing** – Converts tokens into an Abstract Syntax Tree (AST).
- **AST Generation** – Represents the structure of expressions and programs.
- **Compilation** – Translates AST into bytecode for a custom virtual machine.
- **Interpreter** – Executes programs with dynamic type checking (Part B1).
- **Support for**:
  - Arithmetic operations (`+`, `-`, `*`, `/`)
  - Logical operations (`&&`, `||`, `!`)
  - Variable declarations (`let`)
  - Conditional expressions (`if-then-else`)
  - Function definitions and calls
  - `read()` and `write()` for I/O

## Structure

- `compiler.fs` – The compiler logic
- `interpreter.fs` – The interpreter logic
- `Syntax.fs` – AST and data types
- `Parser.fs` – The parser
- `Tests.fs` – Unit tests and sample programs
- `vm.dll` – Custom virtual machine to execute bytecode

## How to Run

1. Open the project in an F# environment (like Visual Studio or VS Code with Ionide).
2. Compile the code.
3. Run tests or execute programs using the virtual machine or the interpreter.

---

This project was created for the FPLI course (Spring 2025) at Roskilde University as a hands-on exercise in building compilers and interpreters.
