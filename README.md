# FooBarBaric: Competitive Programming

Authors: Cadet Owen Lang, Cadet Christian Preti  
Date: October 16th, 2026

## Overview

This is FooBarBaric! It is a programming language centered around the code actively fighting the user. First and foremost: this code should NOT be used for any actual development! It is experimental, and it is designed to help teach the process of creating a programming language (while also intentionally being horrible!).

A few key elements of this program include different 'stages' in which it runs; deterministic 'quick time events' (QTE) as seen in the tests; different interpretations of the code based on what stage you are in; a built-in 'heartbeat' functionality that requires a function to be called every six statements (FooBar()), which also triggers a QTE; and different combinations of 'foo' and 'bar'! Download this repo if you want to test your programming skills :D

A few notes:
*must include exactly six executed statements between heartbeats (calling 'FooBar()')
*there is a 1000-step execution limit
*there are deterministic QTE inputs and no random seed requirement

## Files

```text
FooBarBaric/
├── examples/                         => collection of example 'fbb' files that successfully run in our language
│   ├── absolute-negative.fbb
│   ├── absolute-positive.fbb
│   ├── capacity-three.fbb
│   ├── capacity-two.fbb
│   ├── function-demo.fbb
│   └── phase-demo.fbb
├── project/                          => specifies sbt version
│   └── build.properties
├── scripts/                          => use when testing for mutation testing
│   ├── results.txt                   => the terminal output showing 3 failed tests after mutation
│   └── run-mutation-test.ps1         => mutation file
├── src/                              => primary source code for language
│   ├── main/scala/foobarbaric/       => houses the components behind FooBarBaric!
│   │   ├── AST.scala
│   │   ├── FooBarDemo.scala
│   │   ├── FooBarFunctionDemo.scala
│   │   ├── FooBarOrdinaryDemo.scala
│   │   ├── FooBarParser.scala
│   │   ├── Interpreter.scala
│   │   ├── RuntimeState.scala
│   │   ├── StatementInterpreter.scala
│   │   └── Statements.scala
│   └── test/scala/                   => houses the primary test files
│       ├── FooBarFunctionTests.scala
│       ├── FooBarOrdinaryTests.scala
│       ├── FooBarParserTests.scala
│       ├── InterpreterTests.scala
│       ├── RuntimeStateTests.scala
│       └── StatementInterpreterTests.scala
├── .gitignore                        => simple git ignore file
├── README.md                         => this right here :D
├── build.sbt                         => specifies how to build the file
└── main.scala                        => the original parser/eval (problem 1)
```

## Running

To run, first ensure that you have an up-to-date version of sbt (can be found at [https://www.scala-sbt.org/download/](https://www.scala-sbt.org/download/)) and Java (can be found at [https://www.oracle.com/java/technologies/javase/21-0-9-relnotes.html](https://www.oracle.com/java/technologies/javase/21-0-9-relnotes.html)). This language was developed and tested with Java at version 21.0.9 and sbt at version 2.0.10. Additionally, you will need Git to clone the project (unless you download a zip).

### Cloning the project

```powershell
git clone https://github.com/owenlang05/FooBarBaric.git
cd FooBarBaric
```

### Compile

```powershell
sbt --no-server compile
```

//ensure you see [success] at end!

### Run the test suites

```powershell
sbt --no-server "Test / testOnly foobarbaric.RuntimeStateTests"
sbt --no-server "Test / testOnly foobarbaric.InterpreterTests"
sbt --no-server "Test / testOnly foobarbaric.StatementInterpreterTests"
sbt --no-server "Test / testOnly foobarbaric.FooBarParserTests"
sbt --no-server "Test / testOnly foobarbaric.FooBarFunctionTests"
sbt --no-server "Test / testOnly foobarbaric.FooBarOrdinaryTests"
```

//43 tests in total!

### Run FooBarBaric programs

**phase-shifting example**

```powershell
sbt --no-server "runMain foobarbaric.runFooBarDemo"
```

**functions and returns example**

```powershell
sbt --no-server "runMain foobarbaric.runFooBarFunctionDemo"
```

**ordinary programming tasks with inputs**

```powershell
sbt --no-server "runMain foobarbaric.runFooBarOrdinaryDemos"
```

### Reproduce mutation example

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\run-mutation-test.ps1
```

## Dependencies
// Requires no paid services/API keys!

## I/O

There are deterministic QTEs within this language; additionally, this language will take normal input and print outputs to screen (as long as it follows the program structure requirements!).

## Test Cases

All of the test cases can be found within 'test/scala'.

## Contact

If you find any issues running the program, please contact Lang at:  
owen.lang@westpoint.edu

or Preti at:  
christian.preti@westpoint.edu
