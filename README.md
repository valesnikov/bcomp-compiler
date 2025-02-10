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
a := 3 /*comment*/
b := 45 //line comment

while a <= 6 { 
    a = a + 2
}

if a != 16 {
    a = <<a + (a & b) - (a | b) + >>a + (-a) + ~b
} else {
    a = -4
}

goto skip
a = 0
skip:

return a + b
```
All values represent integer signed 16 bit numbers (int16)
### Statements
* `{var} := {expr}` - assignment operator
* `{var} = {expr}` - modifying operator
* `goto {label}` - continues execution from the given label
* `{label}:` - label to which goto jumps
* `return {expr}` - puts the result of the expression into the AC register and stops execution with HLT
* `if {logic_expr} {stmts} [else {stmts}] ` - conditional execution
* `while {logic_expr} {stmts}` - preconditioned loop

### Logic expressions
* `{expr} == {expr}` - equally
* `{expr} != {expr}` - not equal
* `{expr} > {expr}` - greater
* `{expr} < {expr}` - lower
* `{expr} >= {expr}` - greater or equally
* `{expr} <= {expr}` - lower or equally

### Expressions (prioritized)
* `+{expr}` - does nothing
* `-{expr}` - negative value
* `~{expr}` - bitwise not
* `<<{expr}` - bitwise left shift (*2)
* `>>{expr}` - bitwise right shift (/2)
*
* `{expr} + {expr}` - sum
* `{expr} - {expr}` - difference
*
* `{expr} | {expr}` - bitwise or 
* `{expr} & {expr}` - bitwise and