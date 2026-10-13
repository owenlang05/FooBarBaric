package foobarbaric

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import Value.*
import LanguageError.*

/** Part II: runnable source programs with two distinct inputs per task. */
class FooBarOrdinaryTests extends AnyFunSuite:

    private def parsed(source: String): Stmt =
        val result = FooBarParser.parse(source)
        assert(result.isRight, result.toString)
        result.toOption.get

    private def fromFile(fileName: String): Stmt =
        val handle = Source.fromFile(s"examples/$fileName", "UTF-8")
        val source = try handle.mkString finally handle.close()
        parsed(source)

    private def runFile(fileName: String, qtes: List[Boolean]): StatementResult =
        StatementInterpreter.run(fromFile(fileName), qtes)

    test("Absolute value handles negative input in Bar phase"):
        val result = runFile("absolute-negative.fbb", List(true))
        assert(result.outcome == Right(()))
        assert(result.state.env.get("FooBar7") == Some(IntV(3)))
        assert(result.state.runtime.phase == Phase.Bar)
        assert(result.state.runtime.comboPoints == 1)
        assert(result.state.runtime.statementsSinceHeartbeat == 2)

    test("Absolute value handles positive input in Bar phase"):
        val result = runFile("absolute-positive.fbb", List(true))
        assert(result.outcome == Right(()))
        assert(result.state.env.get("FooBar7") == Some(IntV(8)))
        assert(result.state.runtime.phase == Phase.Bar)
        assert(result.state.runtime.comboPoints == 1)

    test("Capacity doubling input 2 repeats twice and produces 8"):
        val result = runFile("capacity-two.fbb", List(true, true))
        assert(result.outcome == Right(()))
        assert(result.state.env.get("FooBar1") == Some(IntV(8)))
        assert(result.state.runtime.phase == Phase.FooBar)
        assert(result.state.runtime.comboPoints == 1)
        assert(result.state.runtime.statementsSinceHeartbeat == 6)

    test("Capacity doubling input 3 repeats once and produces 6"):
        val result = runFile("capacity-three.fbb", List(true, true))
        assert(result.outcome == Right(()))
        assert(result.state.env.get("FooBar1") == Some(IntV(6)))
        assert(result.state.runtime.phase == Phase.FooBar)
        assert(result.state.runtime.comboPoints == 2)
        assert(result.state.runtime.statementsSinceHeartbeat == 4)

    test("Failed QTE shifts phase but leaves comparison unaffordable"):
        val result = runFile("absolute-negative.fbb", List(false))
        assert(result.outcome == Left(InsufficientComboPoints(1, 0)))
        assert(result.state.runtime.phase == Phase.Bar)
        assert(result.state.runtime.comboPoints == 0)
        assert(!result.state.env.contains("FooBar7"))

    test("Original phase demo combines precedence and Bar inversion"):
        val result = runFile("phase-demo.fbb", List(true))
        assert(result.outcome == Right(()))
        assert(result.state.env.get("FooBar1") == Some(IntV(14)))
        assert(result.state.env.get("FooBar7") == Some(IntV(20)))
        assert(result.state.env.get("FooBar8") == Some(IntV(42)))
        assert(result.state.runtime.comboPoints == 1)

    test("Original function demo combines QTE funding with call and return"):
        val result = runFile("function-demo.fbb", List(true, true, true))
        assert(result.outcome == Right(()))
        assert(result.state.env.get("FooBar50") == Some(IntV(6)))
        assert(result.state.runtime.phase == Phase.Foo)
        assert(result.state.runtime.comboPoints == 1)

    test("A premature heartbeat is a controlled error"):
        val result = StatementInterpreter.run(parsed("FooBar1 foobar 7\nFooBar()"), List(true))
        assert(result.outcome == Left(HeartbeatTooEarly(1)))

    test("A missing simulated QTE outcome is a controlled error"):
        val source = (1 to 6).map(i => s"FooBar$i foobar $i").mkString("\n") + "\nFooBar()"
        val result = StatementInterpreter.run(parsed(source))
        assert(result.outcome == Left(MissingQTEOutcome))

    test("A seventh ordinary statement without heartbeat is rejected"):
        val source = (1 to 7).map(i => s"FooBar$i foobar $i").mkString("\n")
        val result = StatementInterpreter.run(parsed(source))
        assert(result.outcome == Left(HeartbeatRequired))
        assert(result.state.runtime.statementsSinceHeartbeat == 6)
