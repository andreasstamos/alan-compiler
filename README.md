# 🚀 **A Compiler for the Alan Programming Language**  

## Implemented in OCaml, targeting LLVM 15 Intermediate Representation (IR) as the output instruction set.

![Banner](/assets/banner.png)

### ✨ Features

1. 📝 Nested and Multi-Line Comments
2. 🛡 Strong Typing and Type-Checking
3. 🔁 Nested Functions with Local Scopes
4. 📥 Reference Parameter Passing
5. ⚡️ LLVM-based Optimization
6. 🔧 *Lexer Generator (**under development**): A generic lexer generator developed from scratch using Haskell.
Regular expressions are processed using Thompson's construction to build NFAs, followed by powerset construction to convert NFAs to DFAs, and then DFA minimization. It generates OCaml code from a file format similar to Flex, but with fewer features. Currently, it can generate working Lexers for a small set of rules, but a memory usage explosion issue prevents its application to the full set of Alan tokens. It may be included in a future release once the issue is resolved.*

### ⚡ Quick Start

1. **Install opam and clang-15 using your favourite package manager.**

`apt install opam clang-15`

If the package `clang-15` is not found, you can add the appropriate repository for your OS and for LLVM 15 from [https://apt.llvm.org](https://apt.llvm.org).

2. **Initialize OPAM.**

`opam init`

If you choose not to modify your `.profile` file (which is likely the best option if you plan to use `opam` only for compiling the Alan compiler), you will need to complete Step 4 before the compilation of the compiler.

2. **Create an OPAM switch using OCaml 5.1.0.**

`opam switch create alanc 5.1.0`

3. **Install using OPAM the OCaml requirements.**

`opam install ocamlfind llvm.15.0.7+nnp-2`

You will be prompted to install some additional packages automatically using your system's package manager. You can either accept this, or install the packages that are stated manually.

4. **Ensure your shell has the required environment variables.**

`eval $(opam env)`

5. **Run `make` to compile the compiler :)**

### 🛠 Usage

```
$ ./alanc --help
Usage: ./alanc [options] [filename]
Options:
  -O           Enable optimizations
  -o <file>    Specify output executable filename (default: ./a.out)
  -f           Read input from stdin and output to stdout
  -i           Read input from stdin and print LLVM IR to stdout
  -h, --help   Show this help message
```

_**Note:**  When executing the `alanc` executable from a different directory, ensure that both the `global.aland` declarations file and the `lib-alan` directory are present in the directory where the command is executed._

### 📂 Examples

1. **Basic compilation**

`./alanc program.alan`. This compiles program.alan and produces an executable `a.out`.

2. **Optimised compilation**

`./alanc -O program.alan`. This enables optimizations.

3. **Custom output filename**

`./alanc -o output_name program.alan`. This compiles the code and outputs an executable with the specified name.

4. **Read from stdin, output final code to stdout**

`./alanc -f < program.alan`

5. **Read from stdin, output IR code to stdout**

`./alanc -i < program.alan`

---

© 2024 Andreas Stamos, Harris Platanos. All rights reserved.
