package foobarbaric

import org.scalatest.funsuite.AnyFunSuite
import Value.*

class FooBarParserTests extends AnyFunSuite:

    test("Original source syntax changes arithmetic precedence after QTE"):
        val source = """
            FooBar1 foobar 2 + 3 * 4
            FooBar2 foobar 0
            FooBar3 foobar 0
            FooBar4 foobar 0
            FooBar5 foobar 0
            FooBar6 foobar 0
            FooBar()
            FooBar7 foobar 2 + 3 * 4
        """

        val parsed = FooBarParser.parse(source)
        assert(parsed.isRight, parsed.toString)

        val result = StatementInterpreter.run(parsed.toOption.get, List(true))
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar1") == IntV(14))
        assert(result.state.env("FooBar7") == IntV(20))
        assert(result.state.runtime.phase == Phase.Bar)

    test("Bar phase keyword barfoo parses as If, then runtime inverts it"):
        val source = """
            FooBar1 foobar 1
            FooBar2 foobar 2
            FooBar3 foobar 3
            FooBar4 foobar 4
            FooBar5 foobar 5
            FooBar6 foobar 6
            FooBar()
            barfoo Foo FooBar1 < 99 Bar
            Foo
                FooBar8 foobar 999
            Bar
            foofoobarbar
            Foo
                FooBar8 foobar 42
            Bar
        """

        val parsed = FooBarParser.parse(source)
        assert(parsed.isRight, parsed.toString)

        val result = StatementInterpreter.run(parsed.toOption.get, List(true))
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar8") == IntV(42))
        assert(result.state.runtime.comboPoints == 1)

    test("FooBar phase arithmetic is right associative"):
        val source = """
            FooBar1 foobar 0
            FooBar2 foobar 0
            FooBar3 foobar 0
            FooBar4 foobar 0
            FooBar5 foobar 0
            FooBar6 foobar 0
            FooBar()
            FooBar7 foobar 0
            FooBar8 foobar 0
            FooBar9 foobar 0
            FooBar10 foobar 0
            FooBar11 foobar 0
            FooBar12 foobar 0
            FooBar()
            FooBar13 foobar 20 - 5 - 2
        """

        val parsed = FooBarParser.parse(source)
        assert(parsed.isRight, parsed.toString)

        val result = StatementInterpreter.run(
            parsed.toOption.get,
            List(true, true)
        )
        assert(result.outcome == Right(()))
        assert(result.state.env("FooBar13") == IntV(17))
        assert(result.state.runtime.phase == Phase.FooBar)
