# VipingWhiley
A transpiler to convert Whiley code to Viper code.
Both [Whiley](https://whiley.org/) and [Viper](https://viper.ethz.ch/) are verification languages to proof algorithms using mathematical constaints.

The language specification of Whiley can be found [here](https://whiley.org/pdfs/WhileyLanguageSpec.pdf).
The parser of the transpiler is closely derived from the languague grammatic from the specification.

Since this project is a university project, the scope is greatly constrained and only a subset of Whiley's language features are supported.
The programming language for the code base is [Scala](https://scala-lang.org/) and the [cats-parse](https://typelevel.org/cats-parse/) library is used to build the parser.

## Build & Run
Since Scala is used, `sbt` is the build tool.
There is an interactive mode, where you can run the program using `run file.whiley` in the interactive shell.
The JAR can be build with `assembly`.

Of course you can run the program without the interactive shell:
```
sbt "run file.whiley"
```

And you can build the JAR file with, which is being stored at `target/`:
```
sbt assembly
```

### Usage
VipingWhiley expects a single command line argument, the path to the whiley file, which is to be transpiled.
The generated Viper code is printed to `stdout`, while errors and warnings are printed to `stderr`.

```
# Generate and print Viper code
java -jar VipingWhiley-assembly-0.1.0-SNAPSHOT.jar file.whiley

# Save Viper code to file
java -jar VipingWhiley-assembly-0.1.0-SNAPSHOT.jar file.whiley > file.vpr
```

## Whiley Example Programs
In the folders `whiley-programs/with-verification` and `whiley-programs/without-verification` are example programs located.
Respectively with or without verification features.

VipingWhiley is able to process all of those examples correctly.

## Project Constraints
At the beginning of the project, a few constraints have been defined, to specify a feasable scope for this small university project.
Basically, the most important language features have been selected to be implemented and the more advanced features are outside of the scope.

The list of constraints:
1. Support primitive types: `int`, `bool`
   - Do not support unions, records and lists
2. Support methods and functions.
3. Do not support classes.
4. Support `ensures` and `requires`
   - Do not support type invariants, etc
5. Support conditional programming (if-else).
6. Do not support loops.

## Challenges During Implementation
While developing the the code and adding new parser functions, combining the parser functions sometimes rose new problems, even though the parser functions for themselfs worked correctly.
At some points the order of the functions had to be adapted, in other cases `backtrack` is needed.
For the recursive parser functions both, the order and using backtrack, had to be fine tuned, so the input is parsed correctly.

In Whiley, code blocks are defined by the indentation of the statements.
Surely, there are different solutions to determine where a code block starts, ends, and where it belongs.
The solution chosen parses every block / statement as deep as possible, but stores the indentation of a statement.
In a second pass, the indentation is compared to the block indentation and statements are being reassigned to parent blocks, if their indentation is too small for their current block.
This method results in correctly structured code blocks.

Viper functions cannot have a state.
Therefore, a function may not have variable declarations or if-else-statements in Viper.
Instead of variable declarations, `let` let's you define temporary values.
Since a Viper functions needs a direct return value, if-else cannot be used, but a selection `(e0 ? e1 : e2)` (`e0` is the guard, `e1` the code block for if and `e2` the code bock for else) is possible.
So the if-else statements are being handled specially inside function code blocks.

## Limitations & Missing Features
Unfortunately, empty statements in code blocks cannot be parsed right now, so empty lines must not be present in code blocks in input files.

Viper functions do not have a named return value, but Whiley does.
The return value of Viper functions can be constraint with `ensures` and the identifier `result` for the return value.
Therefore, the identifier `result` must be chosen so Whiley function's constraints are compatible with Viper's constraints system.
A future goal could be, to automatically analyze the constraints and replace the identifier of the result value with `result`.
