# Implementing-Programming-Languages
A dive into implementing interpreters and programming languages in OCaml, taking a look at the specific implementation features that dictate languages from the "inside". Includes functional, imperative, untyped, dynamically typed, and statically typed languages.

Resources for the labs, projects, and assessments can be found at the following links
- [Course Website](https://mvanier.github.io/cs131/2023/book/)
- [Course Textbook: Programming Languages: Build, Prove, and Compare](https://www.amazon.com/Programming-Languages-Build-Prove-Compare/dp/110718018X/ref=sr_1_1?keywords=Programming+Languages+Build+Prove+and+Compare&link_code=qs&qid=1680678819&sourceid=Mozilla-search&sr=8-1&ufe=app_do%3Aamzn1.fos.006c50ae-5d4c-4777-9bc0-4513d670b6bc)

## Grade Received: A+

## Implemented Languages
Each lab folder is an individual project that contains an implementation of a programming language in OCaml. The languages for each lab are as follows:
- lab1: imp 
  - Featureless Imperative Language
  - Untyped / Unityped (Only Integers)
  - Big Step Operational Semantics: Direct Evaluation with Simple Environment Model
  - Only Top-Level Functions
- lab2: µscheme (uscheme)
  - Base Mostly Functional Language
  - Dynamically Typed (Units, Booleans, Integers, Pairs/Lists, Functions)
  - Big Step Operational Semantics: Intermediate Interpretation with Single Environment for All Bindings
  - Support for First-Class Functions, Variadic Functions, and Function Closure
- lab3: µscheme+ (uscheme_plus)
  - Mostly Functional Language
  - Dynamically Typed (Units, Booleans, Integers, Pairs/Lists, Functions)
  - Small Step Operational Semantics: Continuation-Passing Interpretation via Stack Frames
  - Support for First-Class Functions, Variadic Functions, Function Closure, Control Flow Operators, and Tail-Call Optimization (TCO)
- lab4: typed imp (typed_imp)
  - Base Typed Imperative Language
  - Statically Typed, Monomorphic Type System (Units, Booleans, Integers, Arrays)
  - Big Step Operational Semantics: Direct Evaluation with Simple Environment Model
  - Big Step Type System Operational Semantics: Direct Type Checking with Simple Environment Model
  - Only Top-Level Functions

## Compile Instructions
To compile any of the languages, cd into the appropriate "lab_#" folder.
  1. cd into the "sexpr" folder for that lab and run make.
  1. cd into the folder with the same name as the language and run make.
  1. To open the terminal REPL, run "rlwrap ./___language_name___"
  1. To run the tests, run "make test" from the same folder as step 2.

## Caltech's Honor Code
"No member of the Caltech community shall take unfair advantage of any other member of the Caltech community."

For any students currently enrolled in this course, the files in this repository are subject to Caltech's Honor Code and as such, should not be referenced until completing the course.
