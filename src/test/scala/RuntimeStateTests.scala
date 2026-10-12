package foobarbaric

import org.scalatest.funsuite.AnyFunSuite

class RuntimeStateTests extends AnyFunSuite:

    test("Successful QTE awards 2 points and advances phase"):
        val initial = RuntimeState()
        val result = RuntimeRules.heartbeat(initial, success = true)

        assert(result.phase == Phase.Bar)
        assert(result.comboPoints == 2)
        assert(result.statementsSinceHeartbeat == 0)

    test("Repeated successful QTEs accumulate points"):
        val first = RuntimeRules.heartbeat(RuntimeState(), true)
        val second = RuntimeRules.heartbeat(first, true)

        assert(second.phase == Phase.FooBar)
        assert(second.comboPoints == 4)

    test("Failed QTE resets points and cycles phase"):
        val initial = RuntimeState(
            phase = Phase.FooBar,
            comboPoints = 4,
            statementsSinceHeartbeat = 5
        )

        val result = RuntimeRules.heartbeat(initial, false)

        assert(result.phase == Phase.Foo)
        assert(result.comboPoints == 0)
        assert(result.statementsSinceHeartbeat == 0)

    test("Attempting an affordable operation spends points"):
        val initial = RuntimeState(comboPoints = 2)
        val result = RuntimeRules.attemptOperation(initial, 1)

        assert(result.state.comboPoints == 1)
        assert(result.outcome == Right(()))

    test("Attempting an operation can spend all remaining points"):
        val initial = RuntimeState(comboPoints = 2)
        val result = RuntimeRules.attemptOperation(initial, 2)

        assert(result.state.comboPoints == 0)
        assert(result.outcome == Right(()))

    test("Insufficient points produce an error without spending"):
        val initial = RuntimeState(comboPoints = 1)
        val result = RuntimeRules.attemptOperation(initial, 3)

        assert(result.state.comboPoints == 1)
        assert(
            result.outcome == Left(
                LanguageError.InsufficientComboPoints(3, 1)
            )
        )
