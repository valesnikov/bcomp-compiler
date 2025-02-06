# bcompc - compiler for ITMO's bcomp-ng
A program that compiles a simple imperative language into bcomp-ng assembler

# Usage
```bash
bcompc #input from stdin, output to stdout
bcompc input.bc #output to stdout
bcompc input.bc output.asm
```
# Install
## Binary
Perhaps the required version is built already on the [releases page](https://github.com/valesnikov/bcomp-compiler/releases)
## Cabal
Open the project directory and type (will need `cabal` and `ghc` installed)
```bash
cabal install
```
## Language
Go-like syntax
```
a := 3
b := 45

a = <<a + (a & b) - (a | b) + >>a + (-a) + ~b

return a + b
```
All values represent integer signed 16 bit numbers (int16)
### Statements
* `{var} := {expr}` - assignment operator
* `{var} = {expr}` - modifying operator
* `return {expr}` - puts the result of the expression into the AC register and stops execution with HLT
### Expressions (prioritized)
* `+{expr}` - does nothing
* `-{expr}` - negative value
* `~{expr}` - bitwise not
* `<<{expr}` - bitwise left shift (*2)
* `<<{expr}` - bitwise right shift (/2)
*
* `{expr} + {expr}` - sum
* `{expr} - {expr}` - difference
*
* `{expr} | {expr}` - bitwise or 
* `{expr} & {expr}` - bitwise and

_Loops and branching will be added soon_
