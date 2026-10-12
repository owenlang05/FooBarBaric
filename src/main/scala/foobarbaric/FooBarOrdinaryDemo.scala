package foobarbaric

import scala.io.Source

/** Demonstrates both ordinary tasks with two inputs each from source files. */
@main def runFooBarOrdinaryDemos(): Unit =
    val demos = List(
        ("Absolute value, input -3", "absolute-negative.fbb", List(true), "FooBar7", 3),
        ("Absolute value, input 8", "absolute-positive.fbb", List(true), "FooBar7", 8),
        ("Double capacity, input 2", "capacity-two.fbb", List(true, true), "FooBar1", 8),
        ("Double capacity, input 3", "capacity-three.fbb", List(true, true), "FooBar1", 6)
    )

    for (label, fileName, qtes, resultName, expected) <- demos do
        val handle = Source.fromFile(s"examples/$fileName", "UTF-8")
        val source = try handle.mkString finally handle.close()

        val parsed = FooBarParser.parse(source)
        parsed match
            case Left(error) =>
                throw new IllegalStateException(s"$label: parse failed: $error")
            case Right(program) =>
                val result = StatementInterpreter.run(program, qtes)
                val observed = result.state.env.get(resultName)
                val wanted = Some(Value.IntV(expected))
                println(s"$label | outcome=${result.outcome} | $resultName=$observed")
                println(s"phase=${result.state.runtime.phase}, combo=${result.state.runtime.comboPoints}, count=${result.state.runtime.statementsSinceHeartbeat}")
                if result.outcome != Right(()) || observed != wanted then
                    throw new IllegalStateException(s"$label: expected $wanted, got $observed")

    println("Four source-to-result ordinary-program checks passed.")
