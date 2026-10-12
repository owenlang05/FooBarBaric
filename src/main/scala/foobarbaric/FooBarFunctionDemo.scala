package foobarbaric

import scala.io.Source

@main def runFooBarFunctionDemo(): Unit =
    val file = Source.fromFile("examples/function-demo.fbb", "UTF-8")
    val source = try file.mkString finally file.close()

    FooBarParser.parse(source) match
        case Left(error) => println(s"Parse error: $error")
        case Right(program) =>
            val result = StatementInterpreter.run(program, List(true, true, true))
            println(s"Outcome: ${result.outcome}")
            println(s"Phase: ${result.state.runtime.phase}")
            println(s"Combo points: ${result.state.runtime.comboPoints}")
            println(s"FooBar50 = ${result.state.env.get("FooBar50")}")
