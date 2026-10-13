package foobarbaric

import scala.io.Source

@main def runFooBarDemo(): Unit =
    val file = Source.fromFile("examples/phase-demo.fbb", "UTF-8")
    val source = try file.mkString finally file.close()

    FooBarParser.parse(source) match
        case Left(error) =>
            println(s"Parse error: $error")
        case Right(program) =>
            // Deterministic test input: this demo has one successful QTE.
            val result = StatementInterpreter.run(program, List(true))
            println(s"Outcome: ${result.outcome}")
            println(s"Phase: ${result.state.runtime.phase}")
            println(s"Combo points: ${result.state.runtime.comboPoints}")
            for name <- List("FooBar1", "FooBar7", "FooBar8") do
                println(s"$name = ${result.state.env.get(name)}")
