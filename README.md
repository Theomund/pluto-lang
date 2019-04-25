# ![Pluto Logo](https://i.imgur.com/tRplGb6.png)

[![Build Status](https://travis-ci.com/Theomund/pluto-lang.svg?branch=master)](https://travis-ci.com/Theomund/pluto-lang)

Pluto is a statically typed imperative programming language built using [Haskell](https://www.haskell.org). Currently, Pluto includes a rudimentary compiler that compiles programs into target-specific assembly code. Under the hood, Pluto uses the [LLVM](https://llvm.org/) compiler toolchain to manage the middle-end and back-end of the compiler.

## Installation 

### Linux (Arch Linux)

1. Install *stack* using *pacman*:
```bash
sudo pacman -S stack
```

2. Clone this repository:
```bash
git clone https://github.com/Theomund/pluto-lang.git
```

3. Initialize *stack* and build the project:
```bash
cd pluto-lang/
stack setup
stack build
```
4. Execute the project using *stack*:
```bash
stack exec pluto-lang-exe
```

## License
[MIT](https://choosealicense.com/licenses/mit/)
