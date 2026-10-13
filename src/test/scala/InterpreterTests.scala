package foobarbaric

import org.scalatest.funsuite.AnyFunSuite
import Expr.*
import Value.*
import LanguageError.*

class InterpreterTests extends AnyFunSuite:

    test("Arithmetic is free"):
        val result = Interpreter.eval(
            Plus(Num(4), Num(5)),
            RuntimeState()
        )

        assert(result.outcome == Right(IntV(9)))
        assert(result.state.comboPoints == 0)

    test("Comparison costs one point"):
        val result = Interpreter.eval(
            Less(Num(3), Num(7)),
            RuntimeState(comboPoints = 2)
        )

        assert(result.outcome == Right(BoolV(true)))
        assert(result.state.comboPoints == 1)

    test("Comparison fails without sufficient points"):
        val result = Interpreter.eval(
            Less(Num(3), Num(7)),
            RuntimeState()
        )

        assert(
            result.outcome ==
                Left(InsufficientComboPoints(1, 0))
        )
        assert(result.state.comboPoints == 0)

    test("And short-circuits but still costs two points"):
        val result = Interpreter.eval(
            And(Bool(false), Less(Num(1), Num(2))),
            RuntimeState(comboPoints = 2)
        )

        assert(result.outcome == Right(BoolV(false)))
        assert(result.state.comboPoints == 0)

    test("And true evaluates the right operand"):
        val result = Interpreter.eval(
            And(Bool(true), Less(Num(1), Num(2))),
            RuntimeState(comboPoints = 2)
        )

        assert(
            result.outcome ==
                Left(InsufficientComboPoints(1, 0))
        )
        assert(result.state.comboPoints == 0)

    test("Nested operations charge independently"):
        val result = Interpreter.eval(
            Less(Less(Num(1), Num(2)), Num(3)),
            RuntimeState(comboPoints = 3)
        )

        assert(result.outcome == Left(ExpectedInt))
        assert(result.state.comboPoints == 1)

    test("A failed operation does not refund its cost"):
        val result = Interpreter.eval(
            Less(Bool(true), Num(5)),
            RuntimeState(comboPoints = 2)
        )

        assert(result.outcome == Left(ExpectedInt))
        assert(result.state.comboPoints == 1)

    test("Division by zero is reported"):
        val result = Interpreter.eval(
            Div(Num(10), Num(0)),
            RuntimeState()
        )

        assert(result.outcome == Left(DivideByZero))

    test("Unbound variables produce controlled errors"):
        val result = Interpreter.eval(
            Var("missing"),
            RuntimeState(comboPoints = 4)
        )

        assert(result.outcome == Left(Unbound("missing")))
        assert(result.state.comboPoints == 4)

    test("Variable lookup uses the environment"):
        val env: Env = Map("x" -> IntV(7))

        val result = Interpreter.eval(
            Plus(Var("x"), Num(3)),
            RuntimeState(),
            env
        )

        assert(result.outcome == Right(IntV(10)))

