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
The JAR can be build with `publishLocal`.

Of course you can run the program without the interactive shell:
```
sbt "run file.whiley"
```

And you can build the JAR file with, which is being stored at `target/`:
```
sbt publishLocal
```

### Usage
VipingWhiley expects a single command line argument, the path to the whiley file, which is to be transpiled.
The generated Viper code is printed to `stdout`, while errors and warnings are printed to `stderr`.

```
# Generate and print Viper code
java -jar vipingwhiley_3.jar file.whiley

# Save Viper code to file
java -jar vipingwhiley_3.jar file.whiley > file.vpr
```

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
TODO

## Missing features
Unfortunately, empty statements in code blocks cannot be parsed right now, so empty lines must not be present in code blocks in input files.
