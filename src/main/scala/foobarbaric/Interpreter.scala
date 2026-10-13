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
    def operationCost(expr: Expr): Int =
        expr match
            case Less(_, _) => 1
            case And(_, _)  => 2
            case Call(_, _) => 2
            case _          => 0

    private def expectInt(
        expr: Expr, state: RuntimeState, env: Env
    ): (RuntimeState, Either[LanguageError, Int]) =
        val result = eval(expr, state, env)
        result.outcome match
            case Left(err) => (result.state, Left(err))
            case Right(IntV(n)) => (result.state, Right(n))
            case Right(_) => (result.state, Left(ExpectedInt))

    private def expectBool(
        expr: Expr, state: RuntimeState, env: Env
    ): (RuntimeState, Either[LanguageError, Boolean]) =
        val result = eval(expr, state, env)
        result.outcome match
            case Left(err) => (result.state, Left(err))
            case Right(BoolV(b)) => (result.state, Right(b))
            case Right(_) => (result.state, Left(ExpectedBool))

    private def expectTwoInts(
        left: Expr, right: Expr, state: RuntimeState, env: Env
    ): (RuntimeState, Either[LanguageError, (Int, Int)]) =
        val (state1, leftResult) = expectInt(left, state, env)
        leftResult match
            case Left(err) => (state1, Left(err))
            case Right(a) =>
                val (state2, rightResult) = expectInt(right, state1, env)
                rightResult match
                    case Left(err) => (state2, Left(err))
                    case Right(b) => (state2, Right((a, b)))

    // Standalone expression evaluator. Calls require the ProgramState in
    // StatementInterpreter, so a bare call here gives a controlled error.
    def eval(expr: Expr, state: RuntimeState, env: Env = Map.empty): EvalResult =
        val attempt = RuntimeRules.attemptOperation(state, operationCost(expr))
        attempt.outcome match
            case Left(err) => EvalResult(attempt.state, Left(err))
            case Right(_) => evalBody(expr, attempt.state, env)

    private def evalBody(expr: Expr, state: RuntimeState, env: Env): EvalResult =
        expr match
            case Num(n) => EvalResult(state, Right(IntV(n)))
            case Bool(b) => EvalResult(state, Right(BoolV(b)))
            case Var(name) =>
                env.get(name) match
                    case Some(value) => EvalResult(state, Right(value))
                    case None => EvalResult(state, Left(Unbound(name)))
            case Plus(left, right) =>
                val (s, outcome) = expectTwoInts(left, right, state, env)
                EvalResult(s, outcome.map { case (a, b) => IntV(a + b) })
            case Minus(left, right) =>
                val (s, outcome) = expectTwoInts(left, right, state, env)
                EvalResult(s, outcome.map { case (a, b) => IntV(a - b) })
            case Mult(left, right) =>
                val (s, outcome) = expectTwoInts(left, right, state, env)
                EvalResult(s, outcome.map { case (a, b) => IntV(a * b) })
            case Div(left, right) =>
                val (s, outcome) = expectTwoInts(left, right, state, env)
                val checked = outcome.flatMap {
                    case (_, 0) => Left(DivideByZero)
                    case (a, b) => Right(IntV(a / b))
                }
                EvalResult(s, checked)
            case Less(left, right) =>
                val (s, outcome) = expectTwoInts(left, right, state, env)
                EvalResult(s, outcome.map { case (a, b) => BoolV(a < b) })
            case And(left, right) =>
                val (s1, leftResult) = expectBool(left, state, env)
                leftResult match
                    case Left(err) => EvalResult(s1, Left(err))
                    case Right(false) => EvalResult(s1, Right(BoolV(false)))
                    case Right(true) =>
                        val (s2, rightResult) = expectBool(right, s1, env)
                        EvalResult(s2, rightResult.map(BoolV.apply))
            case Call(name, _) =>
                EvalResult(state, Left(UndefinedFunction(name)))
