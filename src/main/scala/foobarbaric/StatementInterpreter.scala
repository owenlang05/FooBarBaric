
package foobarbaric

import Stmt.*
import Value.*
import LanguageError.*

case class ProgramState(
    runtime: RuntimeState = RuntimeState(),
    env: Env = Map.empty,
    output: List[Value] = Nil,
    qteResults: List[Boolean] = Nil,
    steps: Int = 0
)

case class StatementResult(
    state: ProgramState,
    outcome: Either[LanguageError, Unit]
)

object StatementInterpreter:

    private val MaxSteps = 1000

    private def failed(
        state: ProgramState,
        error: LanguageError
    ): StatementResult =
        StatementResult(state, Left(error))

    private def succeeded(
        state: ProgramState
    ): StatementResult =
        StatementResult(state, Right(()))

    // Count an executed ordinary statement.
    // Block and FooBar are handled separately.
    private def tick(state: ProgramState): StatementResult =
        if state.steps >= MaxSteps then
            failed(state, StepLimitExceeded)
        else if state.runtime.statementsSinceHeartbeat >= 6 then
            failed(state, HeartbeatRequired)
        else
            val updatedRuntime = state.runtime.copy(
                statementsSinceHeartbeat =
                    state.runtime.statementsSinceHeartbeat + 1
            )
            succeeded(state.copy(
                runtime = updatedRuntime,
                steps = state.steps + 1
            ))

    // Transfer updated combo points back into program state.
    private def evaluate(
        expr: Expr,
        state: ProgramState
    ): (ProgramState, Either[LanguageError, Value]) =
        val result =
            Interpreter.eval(expr, state.runtime, state.env)

        (
            state.copy(runtime = result.state),
            result.outcome
        )

    // Foo/Bar/FooBar semantics are selected at execution time.
    private def conditionIsTrue(
        value: Boolean,
        phase: Phase
    ): Boolean =
        phase match
            case Phase.Foo    => value
            case Phase.Bar    => !value
            case Phase.FooBar => value

    private def executeList(
        statements: List[Stmt],
        state: ProgramState
    ): StatementResult =
        statements match
            case Nil =>
                succeeded(state)

            case head :: tail =>
                val result = execute(head, state)

                result.outcome match
                    case Left(_) => result
                    case Right(_) =>
                        executeList(tail, result.state)

    def run(
        program: Stmt,
        qteResults: List[Boolean] = Nil
    ): StatementResult =
        execute(
            program,
            ProgramState(qteResults = qteResults)
        )

    def execute(
        stmt: Stmt,
        state: ProgramState
    ): StatementResult =
        stmt match

            // A block is a container, not a counted statement.
            case Block(statements) =>
                executeList(statements, state)

            // A heartbeat must follow exactly six statements.
            case FooBar =>
                if state.steps >= MaxSteps then
                    failed(state, StepLimitExceeded)
                else if state.runtime.statementsSinceHeartbeat != 6 then
                    failed(
                        state,
                        HeartbeatTooEarly(
                            state.runtime.statementsSinceHeartbeat
                        )
                    )
                else
                    state.qteResults match
                        case Nil =>
                            failed(state, MissingQTEOutcome)

                        case success :: remaining =>
                            val updatedRuntime =
                                RuntimeRules.heartbeat(
                                    state.runtime,
                                    success
                                )

                            succeeded(state.copy(
                                runtime = updatedRuntime,
                                qteResults = remaining,
                                steps = state.steps + 1
                            ))

            case Assign(name, expr) =>
                val counted = tick(state)

                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val (next, result) =
                            evaluate(expr, counted.state)

                        result match
                            case Left(err) => failed(next, err)
                            case Right(value) =>
                                succeeded(next.copy(
                                    env = next.env.updated(name, value)
                                ))

            case Print(expr) =>
                val counted = tick(state)

                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val (next, result) =
                            evaluate(expr, counted.state)

                        result match
                            case Left(err) => failed(next, err)
                            case Right(value) =>
                                succeeded(next.copy(
                                    output = next.output :+ value
                                ))

            case If(condition, thenBranch, elseBranch) =>
                val counted = tick(state)

                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        // Capture phase when this condition executes.
                        val phase = counted.state.runtime.phase
                        val (next, result) =
                            evaluate(condition, counted.state)

                        result match
                            case Left(err) =>
                                failed(next, err)

                            case Right(BoolV(value)) =>
                                if conditionIsTrue(value, phase) then
                                    execute(thenBranch, next)
                                else
                                    elseBranch match
                                        case Some(branch) =>
                                            execute(branch, next)
                                        case None =>
                                            succeeded(next)

                            case Right(_) =>
                                failed(next, ExpectedBool)

            case While(condition, body) =>
                // Re-entering execute counts every condition check,
                // including the final check that exits the loop.
                val counted = tick(state)

                counted.outcome match
                    case Left(_) => counted
                    case Right(_) =>
                        val phase = counted.state.runtime.phase
                        val (next, result) =
                            evaluate(condition, counted.state)

                        result match
                            case Left(err) =>
                                failed(next, err)

                            case Right(BoolV(value)) =>
                                if conditionIsTrue(value, phase) then
                                    val bodyResult = execute(body, next)

                                    bodyResult.outcome match
                                        case Left(_) =>
                                            bodyResult
                                        case Right(_) =>
                                            execute(stmt, bodyResult.state)
                                else
                                    succeeded(next)

                            case Right(_) =>
                                failed(next, ExpectedBool)
