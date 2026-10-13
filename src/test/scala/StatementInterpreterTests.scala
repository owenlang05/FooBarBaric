
package foobarbaric

import org.scalatest.funsuite.AnyFunSuite
import Stmt.*
import Expr.*
import Value.*
import Phase.Bar
import LanguageError.*

class StatementInterpreterTests extends AnyFunSuite:

    test("Six statements followed by a successful heartbeat"):
        val program = Block(List(
            Assign("x", Num(1)),
            Print(Var("x")),
            Assign("x", Num(2)),
            Print(Var("x")),
            Assign("x", Num(3)),
            Print(Var("x")),
            FooBar
        ))

        val result =
            StatementInterpreter.run(program, List(true))

        assert(result.outcome == Right(()))
        assert(result.state.output ==
            List(IntV(1), IntV(2), IntV(3)))
        assert(result.state.runtime.phase == Bar)
        assert(result.state.runtime.comboPoints == 2)
        assert(result.state.runtime.statementsSinceHeartbeat == 0)

    test("Bar phase inverts an If condition"):
        val program = Block(List(
            Assign("x", Num(1)),
            Print(Num(0)),
            Print(Num(0)),
            Print(Num(0)),
            Print(Num(0)),
            Print(Num(0)),
            FooBar,
            If(
                Less(Num(1), Num(2)),
                Print(Num(99)),
                Some(Print(Num(42)))
            )
        ))

        val result =
            StatementInterpreter.run(program, List(true))

        assert(result.outcome == Right(()))
        assert(result.state.output.last == IntV(42))
        assert(result.state.runtime.comboPoints == 1)

    test("A seventh statement requires a heartbeat"):
        val program = Block(
            List.fill(7)(Print(Num(1)))
        )

        val result = StatementInterpreter.run(program)

        assert(result.outcome == Left(HeartbeatRequired))
        assert(result.state.output.length == 6)

    test("A heartbeat cannot occur early"):
        val program = Block(List(
            Print(Num(1)),
            FooBar
        ))

        val result =
            StatementInterpreter.run(program, List(true))

        assert(result.outcome ==
            Left(HeartbeatTooEarly(1)))

    test("A failed QTE resets points but advances phase"):
        val program = Block(
            List.fill(6)(Print(Num(1))) :+ FooBar
        )

        val result =
            StatementInterpreter.run(program, List(false))

        assert(result.outcome == Right(()))
        assert(result.state.runtime.phase == Bar)
        assert(result.state.runtime.comboPoints == 0)
    

    test("A While loop performs repeated computation"):
        val program = Block(List(
            Assign("x", Num(0)),
            While(
                Less(Var("x"), Num(2)),
                Assign("x", Plus(Var("x"), Num(1)))
            ),
            FooBar,
            Print(Var("x"))
        ))

        val state = ProgramState(
            runtime = RuntimeState(comboPoints = 3),
            qteResults = List(true)
        )

        val result =
            StatementInterpreter.execute(program, state)

        assert(result.outcome == Right(()))
        assert(result.state.output == List(IntV(2)))
        assert(result.state.runtime.comboPoints == 2)
        assert(result.state.runtime.phase == Bar)
        assert(result.state.runtime.statementsSinceHeartbeat == 1)
