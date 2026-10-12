
package foobarbaric

import Expr.*
import Value.*
import LanguageError.*

type Env = Map[String, Value]

case class EvalResult(
    state: RuntimeState,
    outcome: Either[LanguageError, Value]
)

object Interpreter:

    // Determines the cost of attempting an operation.
    def operationCost(expr: Expr): Int =
        expr match
            case Plus(_, _) => 0
            case Div(_, _)  => 0
            case Less(_, _) => 1
            case And(_, _)  => 2
            case _          => 0

    // Evaluate an expression and require an integer.
    // Preserve the resulting state even on a type error.
    private def expectInt(
        expr: Expr,
        state: RuntimeState,
        env: Env
    ): (RuntimeState, Either[LanguageError, Int]) =

        val result = eval(expr, state, env)

        result.outcome match
            case Left(err) =>
                (result.state, Left(err))
            case Right(IntV(n)) =>
                (result.state, Right(n))
            case Right(_) =>
                (result.state, Left(ExpectedInt))

    // Evaluate an expression and require a Boolean.
    private def expectBool(
        expr: Expr,
        state: RuntimeState,
        env: Env
    ): (RuntimeState, Either[LanguageError, Boolean]) =

        val result = eval(expr, state, env)

        result.outcome match
            case Left(err) =>
                (result.state, Left(err))
            case Right(BoolV(b)) =>
                (result.state, Right(b))
            case Right(_) =>
                (result.state, Left(ExpectedBool))

    // Evaluate two integer operands from left to right.
    // Stop immediately if either operand fails.
    private def expectTwoInts(
        left: Expr,
        right: Expr,
        state: RuntimeState,
        env: Env
    ): (RuntimeState, Either[LanguageError, (Int, Int)]) =

        val (state1, leftResult) = expectInt(left, state, env)

        leftResult match
            case Left(err) =>
                (state1, Left(err))

            case Right(a) =>
                val (state2, rightResult) =
                    expectInt(right, state1, env)

                rightResult match
                    case Left(err) =>
                        (state2, Left(err))

                    case Right(b) =>
                        (state2, Right((a, b)))

    // Public entry point.
    // Charge the operation before evaluating its contents.
    def eval(
        expr: Expr,
        state: RuntimeState,
        env: Env = Map.empty
    ): EvalResult =

        val attempt =
            RuntimeRules.attemptOperation(
                state,
                operationCost(expr)
            )

        attempt.outcome match
            case Left(err) =>
                EvalResult(attempt.state, Left(err))

            case Right(_) =>
                evalBody(expr, attempt.state, env)

    // Perform the expression's actual computation.
    // The operation cost has already been deducted.
    private def evalBody(
        expr: Expr,
        state: RuntimeState,
        env: Env
    ): EvalResult =

        expr match

            case Num(n) =>
                EvalResult(state, Right(IntV(n)))

            case Bool(b) =>
                EvalResult(state, Right(BoolV(b)))

            case Var(name) =>
                env.get(name) match
                    case Some(value) =>
                        EvalResult(state, Right(value))
                    case None =>
                        EvalResult(state, Left(Unbound(name)))

            case Plus(left, right) =>
                val (nextState, result) =
                    expectTwoInts(left, right, state, env)

                result match
                    case Left(err) =>
                        EvalResult(nextState, Left(err))
                    case Right((a, b)) =>
                        EvalResult(
                            nextState,
                            Right(IntV(a + b))
                        )

            case Div(left, right) =>
                val (nextState, result) =
                    expectTwoInts(left, right, state, env)

                result match
                    case Left(err) =>
                        EvalResult(nextState, Left(err))

                    case Right((_, 0)) =>
                        EvalResult(nextState, Left(DivideByZero))

                    case Right((a, b)) =>
                        EvalResult(
                            nextState,
                            Right(IntV(a / b))
                        )
                        
            case Minus(left, right) =>
                val (nextState, result) =
                    expectTwoInts(left, right, state, env)
                EvalResult(
                    nextState,
                    result.map { case (a, b) => IntV(a - b) }
                )

            case Mult(left, right) =>
                val (nextState, result) =
                    expectTwoInts(left, right, state, env)
                EvalResult(
                    nextState,
                    result.map { case (a, b) => IntV(a * b) }
                )

            case Less(left, right) =>
                val (nextState, result) =
                    expectTwoInts(left, right, state, env)

                result match
                    case Left(err) =>
                        EvalResult(nextState, Left(err))

                    case Right((a, b)) =>
                        EvalResult(
                            nextState,
                            Right(BoolV(a < b))
                        )

            case And(left, right) =>
                val (state1, leftResult) =
                    expectBool(left, state, env)

                leftResult match
                    case Left(err) =>
                        EvalResult(state1, Left(err))

                    case Right(false) =>
                        // Short-circuit: skip the right operand.
                        EvalResult(
                            state1,
                            Right(BoolV(false))
                        )

                    case Right(true) =>
                        val (state2, rightResult) =
                            expectBool(right, state1, env)

                        rightResult match
                            case Left(err) =>
                                EvalResult(state2, Left(err))

                            case Right(b) =>
                                EvalResult(
                                    state2,
                                    Right(BoolV(b))
                                )
