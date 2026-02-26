# bcompc - compiler for ITMO's bcomp-ng
A program that compiles a simple imperative language into bcomp-ng assembler

# Usage
```
Usage: bcompc [INPUT] [OUTPUT]

  Compile source code to ITMO's bcomp-ng assembly

Available options:
  INPUT                    Path to source file (default: stdin)
  OUTPUT                   Path to output file (default: stdout)
  -h,--help                Show this help text
```
# Install
Open the project directory and type (will need `cabal` and `ghc` installed)
```bash
cabal install
```
# Language
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
## Statements
* `{var} := {expr}` - assignment operator
* `{var} = {expr}` - modifying operator
* `goto {label}` - continues execution from the given label
* `{label}:` - label to which goto jumps
* `return {expr}` - puts the result of the expression into the AC register and stops execution with HLT
* `if {logic_expr} {stmts} [else {stmts}] ` - conditional execution
* `while {logic_expr} {stmts}` - preconditioned loop
* `out {port} {expr}` - writes expression value to output port

## Logic expressions
* `{expr} == {expr}` - equally
* `{expr} != {expr}` - not equal
* `{expr} > {expr}` - greater
* `{expr} < {expr}` - lower
* `{expr} >= {expr}` - greater or equally
* `{expr} <= {expr}` - lower or equally

## Expressions (prioritized)
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
* `in {port}` - reads value from input port
