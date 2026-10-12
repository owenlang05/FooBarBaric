package foobarbaric

import Stmt.*
import Expr.*
import Value.*
import LanguageError.*

case class FunctionDefinition(params: List[String], body: Stmt)

case class ProgramState(
    runtime: RuntimeState = RuntimeState(),
    env: Env = Map.empty,
    output: List[Value] = Nil,
    qteResults: List[Boolean] = Nil,
    steps: Int = 0,
    functions: Map[String, FunctionDefinition] = Map.empty,
    returnValue: Option[Value] = None,
    callDepth: Int = 0
)

case class StatementResult(
    state: ProgramState,
    outcome: Either[LanguageError, Unit]
)

object StatementInterpreter:
    private val MaxSteps = 1000
    private type ValueResult = (ProgramState, Either[LanguageError, Value])
    private type PairResult = (ProgramState, Either[LanguageError, (Int, Int)])

    private def failed(state: ProgramState, err: LanguageError): StatementResult =
        StatementResult(state, Left(err))

    private def succeeded(state: ProgramState): StatementResult =
        StatementResult(state, Right(()))

    private def tick(state: ProgramState): StatementResult =
        if state.steps >= MaxSteps then
            failed(state, StepLimitExceeded)
        else if state.runtime.statementsSinceHeartbeat >= 6 then
            failed(state, HeartbeatRequired)
        else
            succeeded(state.copy(
                runtime = state.runtime.copy(
                    statementsSinceHeartbeat = state.runtime.statementsSinceHeartbeat + 1
                ),
                steps = state.steps + 1
            ))

    private def conditionIsTrue(value: Boolean, phase: Phase): Boolean =
        phase match
            case Phase.Foo => value
            case Phase.Bar => !value
            case Phase.FooBar => value

    // Program-level expression evaluator: same cost rules as Interpreter,
    // but can execute function bodies and carry their runtime effects.
    private def evalExpr(expr: Expr, state: ProgramState): ValueResult =
        val payment = RuntimeRules.attemptOperation(
            state.runtime,
            Interpreter.operationCost(expr)
        )
        val paidState = state.copy(runtime = payment.state)
        payment.outcome match
            case Left(err) => (paidState, Left(err))
            case Right(_) => evalBody(expr, paidState)

    private def expectInt(expr: Expr, state: ProgramState): (ProgramState, Either[LanguageError, Int]) =
        val (s, outcome) = evalExpr(expr, state)
        (s, outcome.flatMap {
            case IntV(n) => Right(n)
            case _ => Left(ExpectedInt)
        })

    private def expectBool(expr: Expr, state: ProgramState): (ProgramState, Either[LanguageError, Boolean]) =
        val (s, outcome) = evalExpr(expr, state)
        (s, outcome.flatMap {
            case BoolV(b) => Right(b)
            case _ => Left(ExpectedBool)
        })

    private def expectTwoInts(
        left: Expr, right: Expr, state: ProgramState
    ): PairResult =
        val (s1, leftResult) = expectInt(left, state)
        leftResult match
            case Left(err) => (s1, Left(err))
            case Right(a) =>
                val (s2, rightResult) = expectInt(right, s1)
                (s2, rightResult.map(b => (a, b)))

    private def evalArguments(
        args: List[Expr], state: ProgramState
    ): (ProgramState, Either[LanguageError, List[Value]]) =
        args match
            case Nil => (state, Right(Nil))
            case head :: tail =>
                val (s1, headResult) = evalExpr(head, state)
                headResult match
                    case Left(err) => (s1, Left(err))
                    case Right(value) =>
                        val (s2, restResult) = evalArguments(tail, s1)
                        (s2, restResult.map(rest => value :: rest))

    private def invoke(
        name: String,
        args: List[Value],
        state: ProgramState
    ): ValueResult =
        state.functions.get(name) match
            case None => (state, Left(UndefinedFunction(name)))
            case Some(fn) if fn.params.length != args.length =>
                (state, Left(ArgumentCountMismatch(fn.params.length, args.length)))
            case Some(fn) =>
                val callerEnv = state.env
                val callerReturn = state.returnValue
                val callerDepth = state.callDepth
                val calleeState = state.copy(
                    // Temporary, simple local parameter environment.
                    env = callerEnv ++ fn.params.zip(args),
                    returnValue = None,
                    callDepth = callerDepth + 1
                )
                val execution = execute(fn.body, calleeState)
                val returned = execution.state.returnValue
                // Runtime, QTE consumption, output, step count survive the call;
                // local variables and the return marker do not escape.
                val restored = execution.state.copy(
                    env = callerEnv,
                    returnValue = callerReturn,
                    callDepth = callerDepth
                )
                execution.outcome match
                    case Left(err) => (restored, Left(err))
                    case Right(_) =>
                        returned match
                            case Some(value) => (restored, Right(value))
                            case None => (restored, Left(MissingReturn(name)))

    private def evalBody(expr: Expr, state: ProgramState): ValueResult =
        expr match
            case Num(n) => (state, Right(IntV(n)))
            case Bool(b) => (state, Right(BoolV(b)))
            case Var(name) =>
                state.env.get(name) match
                    case Some(value) => (state, Right(value))
                    case None => (state, Left(Unbound(name)))
            case Plus(a, b) =>
                val (s, result) = expectTwoInts(a, b, state)
                (s, result.map { case (x, y) => IntV(x + y) })
            case Minus(a, b) =>
                val (s, result) = expectTwoInts(a, b, state)
                (s, result.map { case (x, y) => IntV(x - y) })
            case Mult(a, b) =>
                val (s, result) = expectTwoInts(a, b, state)
                (s, result.map { case (x, y) => IntV(x * y) })
            case Div(a, b) =>
                val (s, result) = expectTwoInts(a, b, state)
                (s, result.flatMap {
                    case (_, 0) => Left(DivideByZero)
                    case (x, y) => Right(IntV(x / y))
                })
            case Less(a, b) =>
                val (s, result) = expectTwoInts(a, b, state)
                (s, result.map { case (x, y) => BoolV(x < y) })
            case And(a, b) =>
                val (s1, leftResult) = expectBool(a, state)
                leftResult match
                    case Left(err) => (s1, Left(err))
                    case Right(false) => (s1, Right(BoolV(false)))
                    case Right(true) =>
                        val (s2, rightResult) = expectBool(b, s1)
                        (s2, rightResult.map(BoolV.apply))
            case Call(name, args) =>
                val (s, evaluated) = evalArguments(args, state)
                evaluated match
                    case Left(err) => (s, Left(err))
                    case Right(values) => invoke(name, values, s)

    private def executeList(statements: List[Stmt], state: ProgramState): StatementResult =
        if state.returnValue.nonEmpty then
            succeeded(state)
        else
            statements match
                case Nil => succeeded(state)
                case head :: tail =>
                    val result = execute(head, state)
                    result.outcome match
                        case Left(_) => result
                        case Right(_) => executeList(tail, result.state)

    def run(program: Stmt, qteResults: List[Boolean] = Nil): StatementResult =
        execute(program, ProgramState(qteResults = qteResults))

    def execute(stmt: Stmt, state: ProgramState): StatementResult =
        stmt match
            case Block(statements) =>
                executeList(statements, state)

            // Function declarations are registered without counting as
            // executed statements; calls execute their bodies later.
            case Function(name, params, body) =>
                succeeded(state.copy(
                    functions = state.functions.updated(name, FunctionDefinition(params, body))
                ))

            case FooBar =>
                if state.steps >= MaxSteps then
                    failed(state, StepLimitExceeded)
                else if state.runtime.statementsSinceHeartbeat != 6 then
                    failed(state, HeartbeatTooEarly(state.runtime.statementsSinceHeartbeat))
                else
                    state.qteResults match
                        case Nil => failed(state, MissingQTEOutcome)
                        case success :: remaining =>
                            succeeded(state.copy(
                                runtime = RuntimeRules.heartbeat(state.runtime, success),
                                qteResults = remaining,
                                steps = state.steps + 1
                            ))

            case Assign(name, expr) =>
                val counted = tick(state)
                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val (next, result) = evalExpr(expr, counted.state)
                        result match
                            case Left(err) => failed(next, err)
                            case Right(value) =>
                                succeeded(next.copy(env = next.env.updated(name, value)))

            case Print(expr) =>
                val counted = tick(state)
                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val (next, result) = evalExpr(expr, counted.state)
                        result match
                            case Left(err) => failed(next, err)
                            case Right(value) =>
                                succeeded(next.copy(output = next.output :+ value))

            case If(condition, thenBranch, elseBranch) =>
                val counted = tick(state)
                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val phase = counted.state.runtime.phase
                        val (next, result) = evalExpr(condition, counted.state)
                        result match
                            case Left(err) => failed(next, err)
                            case Right(BoolV(value)) =>
                                if conditionIsTrue(value, phase) then
                                    execute(thenBranch, next)
                                else
                                    elseBranch match
                                        case Some(branch) => execute(branch, next)
                                        case None => succeeded(next)
                            case Right(_) => failed(next, ExpectedBool)

            case While(condition, body) =>
                val counted = tick(state)
                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val phase = counted.state.runtime.phase
                        val (next, result) = evalExpr(condition, counted.state)
                        result match
                            case Left(err) => failed(next, err)
                            case Right(BoolV(value)) =>
                                if conditionIsTrue(value, phase) then
                                    val bodyResult = execute(body, next)
                                    bodyResult.outcome match
                                        case Left(_) => bodyResult
                                        case Right(_) if bodyResult.state.returnValue.nonEmpty =>
                                            bodyResult
                                        case Right(_) => execute(stmt, bodyResult.state)
                                else succeeded(next)
                            case Right(_) => failed(next, ExpectedBool)

            case Return(expr) =>
                if state.callDepth == 0 then
                    failed(state, ReturnOutsideFunction)
                else
                    val counted = tick(state)
                    counted.outcome match
                        case Left(_) => counted
                        case Right(_) =>
                            val payment = RuntimeRules.attemptOperation(
                                counted.state.runtime, 3
                            )
                            val paid = counted.state.copy(runtime = payment.state)
                            payment.outcome match
                                case Left(err) => failed(paid, err)
                                case Right(_) =>
                                    val (next, value) = evalExpr(expr, paid)
                                    value match
                                        case Left(err) => failed(next, err)
                                        case Right(v) =>
                                            succeeded(next.copy(returnValue = Some(v)))
