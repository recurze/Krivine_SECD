## Krivine-SECD

1. This project contains [abstract machines](https://en.wikipedia.org/wiki/Abstract_machine), [krivine](https://en.wikipedia.org/wiki/Krivine_machine) and [SECD](https://en.wikipedia.org/wiki/SECD_machine), implemented in Ocaml.

2. Krivine implements [Call-by-name](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_name) semantics. We consider Closures as a pair of a Table and expression, where Table is a partial function from variables to Answers(includes value Closures)

3. SECD implements [Call-by-value](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_value) semantics. We consider value closures in the set of answers.


## Motivation

Was created as one of the assignments in one Programming Languages course.

## How to run?

Fire up an ocaml interpreter:
```
$ ocaml
````

Use *#use* to import code from files:
```
# #use "filename.ml";;
```

Form expressions e, following the types declared in [types.ml]() and evaluate using:
```
# krivine (CLOSURE([], e)) [];;
# secd [] [] (CLOSURE([], e)) [];;
```
