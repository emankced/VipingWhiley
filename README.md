# VipingWhiley
A compiler to convert Whiley code to Viper code.
Since this project is a university project, the scope is greatly constrained.

The language specification of Whiley can be found [here](https://whiley.org/pdfs/WhileyLanguageSpec.pdf).

## Project Constraints
 - only support primitive types: `int`, `bool`, `null`
   - do not support unions, records and lists
 - support methods and functions
 - do not support classes
 - only support `ensures` and `requires`
   - do not support type invariants
