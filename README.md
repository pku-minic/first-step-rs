# first-step-rust

Take your first step in writing a compiler, using Rust.

## Building from Source

Make sure the Rust toolchain is installed on your computer, and then run:

```
$ git clone --recursive https://github.com/pku-minic/first-step-rust.git
$ cd first-step-rust
$ cargo build --release
```

## Getting Started

There are some example `first-step` source programs in the `examples` directory, such as `fib.fstep`:

```first-step
# calculate the nth term of the Fibonacci sequence
fib(n) {
  if n <= 2 {
    return 1
  }
  else {
    return fib(n - 1) + fib(n - 2)
  }
}

main() {
  print(fib(input()))
  return 0
}
```

You can evaluate this program using the interpreter by running:

```
$ cargo run --release -- examples/fib.fstep
20
6765
```

Or compile it to RISC-V assembly:

```
$ cargo run --release -- examples/fib.fstep -c -o out.S
$ cat out.S | less
```

## EBNF of first-step

```ebnf
Program       ::= {FunctionDef};
FunctionDef   ::= IDENT "(" [ArgsDef] ")" Block;
ArgsDef       ::= IDENT {"," IDENT};

Block         ::= "{" {Statement} "}";
Statement     ::= IDENT ":=" Expression
                | IDENT "=" Expression
                | FunctionCall
                | IfElse
                | "return" Expression;
IfElse        ::= "if" Expression Block ["else" (IfElse | Block)];

Expression    ::= LOrExpr;
LOrExpr       ::= LAndExpr {"||" LAndExpr};
LAndExpr      ::= EqExpr {"&&" EqExpr};
EqExpr        ::= RelExpr {("==" | "!=") RelExpr};
RelExpr       ::= AddExpr {("<" | "<=") AddExpr};
AddExpr       ::= MulExpr {("+" | "-") MulExpr};
MulExpr       ::= UnaryExpr {("*" | "/" | "%") UnaryExpr};
UnaryExpr     ::= ["-" | "!"] Value;
Value         ::= INTEGER
                | IDENT
                | FunctionCall
                | "(" Expression ")";
FunctionCall  ::= IDENT "(" [Args] ")";
Args          ::= Expression {"," Expression};
```

## License

Copyright (C) 2010-2021 MaxXing. License GPLv3.
