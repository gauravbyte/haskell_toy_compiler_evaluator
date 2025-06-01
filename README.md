
# Haskell Toy Language Compiler

This project implements a **toy programming language** with a **lexer**, **parser**, **typechecker**, and **evaluator**, entirely written in **Haskell**. It demonstrates a minimal yet functional language supporting arithmetic, booleans, functions, and let-bindings.

---

## ✨ Features

- **Tokenizer**: Uses [Alex](https://www.haskell.org/alex/) (`Lexer.x`) to tokenize the source code.
- **Parser**: Uses [Happy](https://www.haskell.org/happy/) (`Grammar.y`) to construct an abstract syntax tree (AST).
- **Typechecker**: Statically checks types of expressions (`Typing.hs`).
- **Evaluator**: Evaluates programs using a CEK machine (`Evaluator.hs`).
- **Interactive and Batch Modes**: Run code in a REPL or from a file.
- **Strict Block Syntax**: Only `{}` blocks can contain multiple statements, separated by `;`.

---

## 📘 Language Syntax

### Statements

```
If Expr Then Stmt Else Stmt Endif
Let Stmt In Stmt
Identifier := Expr
Begin ListOfStmt End
```

### Expressions

```
Arithmetic: +, -, *, /
Booleans: &&, ||, !, <, >, =
Functions: Lambda abstraction, application
Let binding: let (x : Type) = Expr in Expr
Conditionals: if Expr then Expr else Expr
```

### Types

```
Int, Bool, Type -> Type
```

---

## 📂 File Structure

| File            | Description                                      |
|-----------------|--------------------------------------------------|
| `Lexer.x`       | Alex lexer definition                            |
| `Grammar.y`     | Happy parser and AST definitions                 |
| `Typing.hs`     | Static typechecker                               |
| `Evaluator.hs`  | CEK machine evaluator                            |
| `a3.hs`         | Batch mode runner                                |
| `a3term.hs`     | Interactive REPL                                 |
| `test*.txt`     | Sample test cases                                |
| `Makefile`      | Build script                                     |
| `src/`          | Archive of older or experimental code versions   |

---

## 🛠️ Building

To build the project, run:

```bash
make
```

This will:
- Generate lexer and parser
- Compile executables: `a3` (batch mode), `a3term` (interactive)

---

## ▶️ Usage

### Batch Mode

Run a program from a file:

```bash
./a3 test1.txt
```

### Interactive Mode

Start the REPL:

```bash
./a3term
```

Enter expressions interactively at the prompt.

---

## 📄 Example (test1.txt)

### Input

```haskell
if 6 < 9 then (if true then let (x : Int) = 4 + 7 in x + 13 else 0) else 1
```

### Output

```
Parsing: if 6 < 9 then (if true then let (x : Int) = 4 + 7 in x + 13 else 0) else 1
Type Checking Passed with type Int
Evaluates to 24
```

---

## 🧪 Additional Syntax Rules

- Statements must be inside `{}` blocks if more than one.
- Statements within blocks must be separated by `;`.
- Boolean expressions now supported in grammar.

---

## 🧾 Sample Input/Output

### Input (in `input.txt`)

```haskell
if 2 + 4 + 50 then {
  j := 0;
  n := 23 + 65 + 65 * 43 * 43;
} else {
  j := 343 * 532 * 43 + 23 + 54;
}
```

### Output

```
Parsed AST...
Type checking passed...
Evaluates to ...
```

---

## 🧹 Clean Build

To clean all generated files and binaries:

```bash
make clean
```

*Note: Source programs are in the root directory; `src/` contains older versions.*

---

## 🧑‍💻 Author

This project was built as a demonstration of compiler construction using Haskell, showcasing concepts such as tokenization, parsing, typing, and CEK-based evaluation.
