package foobarbaric

import org.scalatest.funsuite.AnyFunSuite
import Value.*
import LanguageError.*

class FooBarFunctionTests extends AnyFunSuite:

    private val addOneSource = """
        foo FooBar10 Foo FooBar1 Bar
        Foo
            barbar FooBar1 + 1
        Bar
        FooBar20 foobar FooBar10 Foo 5 Bar
    """

    private def parsed(source: String): Stmt =
        val result = FooBarParser.parse(source)
        assert(result.isRight, result.toString)
        result.toOption.get

    test("Function argument and return value execute from source"):
        val state = ProgramState(runtime = RuntimeState(comboPoints = 5))
        val result = StatementInterpreter.execute(parsed(addOneSource), state)
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar20") == IntV(6))
        assert(result.state.runtime.comboPoints == 0)
        assert(result.state.runtime.statementsSinceHeartbeat == 2)

    test("Two parameters use the original foo bar foo separator"):
        val source = """
            foo FooBar10 Foo FooBar1 foo bar foo FooBar2 Bar
            Foo
                barbar FooBar1 + FooBar2
            Bar
            FooBar20 foobar FooBar10 Foo 5 foo bar foo 7 Bar
        """
        val state = ProgramState(runtime = RuntimeState(comboPoints = 5))
        val result = StatementInterpreter.execute(parsed(source), state)
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar20") == IntV(12))
        assert(result.state.runtime.comboPoints == 0)

    test("Return stops later statements in a function body"):
        val source = """
            foo FooBar10 Foo FooBar1 Bar
            Foo
                barbar FooBar1
                FooBar55 foobar 999
            Bar
            FooBar20 foobar FooBar10 Foo 8 Bar
        """
        val state = ProgramState(runtime = RuntimeState(comboPoints = 5))
        val result = StatementInterpreter.execute(parsed(source), state)
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar20") == IntV(8))
        assert(!result.state.env.contains("FooBar55"))
        assert(result.state.runtime.statementsSinceHeartbeat == 2)

    test("Return costs are retained when points are insufficient"):
        val state = ProgramState(runtime = RuntimeState(comboPoints = 4))
        val result = StatementInterpreter.execute(parsed(addOneSource), state)
        assert(result.outcome == Left(InsufficientComboPoints(3, 2)))
        assert(result.state.runtime.comboPoints == 2)
        assert(!result.state.env.contains("FooBar20"))

    test("Nested calls pay for every call and return"):
        val source = """
            foo FooBar10 Foo FooBar1 Bar
            Foo
                barbar FooBar1 + 1
            Bar
            foo FooBar11 Foo FooBar2 Bar
            Foo
                barbar FooBar10 Foo FooBar2 Bar
            Bar
            FooBar20 foobar FooBar11 Foo 10 Bar
        """
        val state = ProgramState(runtime = RuntimeState(comboPoints = 10))
        val result = StatementInterpreter.execute(parsed(source), state)
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar20") == IntV(11))
        assert(result.state.runtime.comboPoints == 0)
        assert(result.state.runtime.statementsSinceHeartbeat == 3)

    test("Return outside a function is a controlled error"):
        val result = StatementInterpreter.run(parsed("barbar 7"))
        assert(result.outcome == Left(ReturnOutsideFunction))

    test("A function with no return produces a controlled error"):
        val source = """
            foo FooBar10 Foo Bar
            Foo
                FooBar1 foobar 7
            Bar
            FooBar20 foobar FooBar10 Foo Bar
        """
        val state = ProgramState(runtime = RuntimeState(comboPoints = 2))
        val result = StatementInterpreter.execute(parsed(source), state)
        assert(result.outcome == Left(MissingReturn("FooBar10")))
        assert(result.state.runtime.comboPoints == 0)

    test("Three successful heartbeats fund a normal function call"):
        val header = """
            foo FooBar10 Foo FooBar1 Bar
            Foo
                barbar FooBar1 + 1
            Bar
        """
        val body = (0 until 3).flatMap { round =>
            (1 to 6).map(i => s"FooBar${round * 6 + i + 20} foobar $i") :+
                "FooBar()"
        }.mkString("\n")
        val source = header + "\n" + body + "\nFooBar50 foobar FooBar10 Foo 5 Bar"
        val result = StatementInterpreter.run(parsed(source), List(true, true, true))
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar50") == IntV(6))
        assert(result.state.runtime.phase == Phase.Foo)
        assert(result.state.runtime.comboPoints == 1)
        assert(result.state.runtime.statementsSinceHeartbeat == 2)
