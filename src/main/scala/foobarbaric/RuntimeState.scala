package foobarbaric

enum Phase:
    case Foo, Bar, FooBar

enum LanguageError:
    case InsufficientComboPoints(required: Int, available: Int)

case class RuntimeState(
    phase: Phase = Phase.Foo,
    comboPoints: Int = 0,
    statementsSinceHeartbeat: Int = 0
)

case class AttemptResult(
    state: RuntimeState,
    outcome: Either[LanguageError, Unit]
)

object RuntimeRules:

    // Move to the next phase, cycling indefinitely.
    def nextPhase(phase: Phase): Phase =
        phase match
            case Phase.Foo => Phase.Bar
            case Phase.Bar => Phase.FooBar
            case Phase.FooBar => Phase.Foo

    // Execute a FooBar() heartbeat with a predetermined QTE result.
    def heartbeat(state: RuntimeState, success: Boolean): RuntimeState =
        val newPoints =
            if success then state.comboPoints + 2
            else 0

        state.copy(
            phase = nextPhase(state.phase),
            comboPoints = newPoints,
            statementsSinceHeartbeat = 0
        )

    // Charge for attempting an operation.
    // This function does NOT evaluate the operation itself.
    def attemptOperation(state: RuntimeState, cost: Int): AttemptResult =
        require(cost >= 0, "Operation cost cannot be negative")

        if state.comboPoints < cost then
            AttemptResult(
                state,
                Left(LanguageError.InsufficientComboPoints(
                    cost,
                    state.comboPoints
                ))
            )
        else
            AttemptResult(
                state.copy(comboPoints = state.comboPoints - cost),
                Right(())
            )
