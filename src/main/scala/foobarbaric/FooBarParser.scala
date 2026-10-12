package foobarbaric

import scala.collection.mutable.ArrayBuffer

/**
 * Source parser for the currently executable subset of FooBarBaric.
 * It parses original FooBar syntax; it does not implement functions/return yet.
 * Phase changes here follow QTEs in SOURCE order, as in the class slides.
 */
object FooBarParser:

    private case class ParseFailure(message: String)
        extends RuntimeException(message)

    private val assignment = """(FooBar[0-9]+)\s+foobar\s+(.+)""".r
    private val control = """(foofoo|barfoo|barbar)\s+Foo\s+(.+)\s+Bar""".r
    private val elseIf = """foofoobar\s+Foo\s+(.+)\s+Bar""".r

    // Whitespace inside the two Boolean literals is tolerated.
    private val expressionToken =
        """FooBar[0-9]+|foo\s+bar|bar\s+foo|[0-9]+|&&|\+|-|\*|/|<|Foo|Bar""".r

    def parse(source: String): Either[String, Stmt] =
        val lines = source.linesIterator
            .map(_.trim)
            .filter(line => line.nonEmpty && !line.startsWith("//"))
            .toVector

        try
            Right(new SourceParser(lines).parseProgram())
        catch
            case err: ParseFailure => Left(err.message)

    private def tokenizeExpression(source: String): Vector[String] =
        val tokens = ArrayBuffer.empty[String]
        var rest = source.trim

        while rest.nonEmpty do
            expressionToken.findPrefixOf(rest) match
                case Some(token) =>
                    tokens += token.replaceAll("\\s+", " ")
                    rest = rest.substring(token.length).trim
                case None =>
                    throw ParseFailure(s"Invalid expression near: '$rest'")

        tokens.toVector

    private class ExpressionParser(source: String, phase: Phase):
        private val tokens = tokenizeExpression(source)
        private var index = 0

        private def next(): String =
            if index >= tokens.length then
                throw ParseFailure("Incomplete expression")
            val token = tokens(index)
            index += 1
            token

        private def precedence(operator: String): Int =
            operator match
                case "&&" => 1
                case "<"  => 2
                case "+" | "-" =>
                    if phase == Phase.Bar then 4 else 3
                case "*" | "/" =>
                    if phase == Phase.Bar then 3 else 4
                case _ => -1

        private def primary(): Expr =
            val token = next()
            token match
                case "Foo" =>
                    val inner = expression(0)
                    if next() != "Bar" then
                        throw ParseFailure("Expected Bar to close Foo")
                    inner
                case "foo bar" => Expr.Bool(true)
                case "bar foo" => Expr.Bool(false)
                case "-" => Expr.Minus(Expr.Num(0), primary())
                case value if value.forall(_.isDigit) =>
                    Expr.Num(value.toInt)
                case name if name.matches("FooBar[0-9]+") =>
                    Expr.Var(name)
                case other =>
                    throw ParseFailure(s"Unexpected expression token: '$other'")

        private def build(operator: String, left: Expr, right: Expr): Expr =
            operator match
                case "+"  => Expr.Plus(left, right)
                case "-"  => Expr.Minus(left, right)
                case "*"  => Expr.Mult(left, right)
                case "/"  => Expr.Div(left, right)
                case "<"  => Expr.Less(left, right)
                case "&&" => Expr.And(left, right)
                case other =>
                    throw ParseFailure(s"Unsupported operator: '$other'")

        private def expression(minPrecedence: Int): Expr =
            var left = primary()
            var continue = true

            while continue && index < tokens.length do
                val operator = tokens(index)
                val priority = precedence(operator)
                if priority < minPrecedence then
                    continue = false
                else
                    index += 1
                    // In FooBar phase, repeated operators bind right-to-left.
                    val nextMinimum =
                        if phase == Phase.FooBar then priority
                        else priority + 1
                    val right = expression(nextMinimum)
                    left = build(operator, left, right)

            left

        def parseAll(): Expr =
            val result = expression(0)
            if index != tokens.length then
                throw ParseFailure(
                    s"Unexpected expression token: '${tokens(index)}'"
                )
            result

    private class SourceParser(lines: Vector[String]):
        private var index = 0
        private var phase = Phase.Foo

        private def current: String = lines(index)

        private def requireLine(expected: String): Unit =
            if index >= lines.length || current != expected then
                val found =
                    if index >= lines.length then "end of file" else current
                throw ParseFailure(s"Expected '$expected', found '$found'")
            index += 1

        private def parseExpression(source: String): Expr =
            new ExpressionParser(source, phase).parseAll()

        // In the original slides, the lexer/parser interprets keywords by phase.
        private def keywordRole(keyword: String): String =
            (phase, keyword) match
                case (Phase.Foo, "foofoo")    => "if"
                case (Phase.Foo, "barfoo")    => "while"
                case (Phase.Foo, "barbar")    => "return"
                case (Phase.Bar, "barfoo")    => "if"
                case (Phase.Bar, "barbar")    => "while"
                case (Phase.Bar, "foofoo")    => "return"
                case (Phase.FooBar, "barbar") => "if"
                case (Phase.FooBar, "foofoo") => "while"
                case (Phase.FooBar, "barfoo") => "return"
                case _ =>
                    throw ParseFailure(s"Unknown phase-dependent keyword: $keyword")

        private def parseBlock(depth: Int): Stmt =
            if depth > 3 then
                throw ParseFailure("FooBarbaric allows only three block depths")

            val canonicalOpen = "Foo" * depth
            val found =
                if index >= lines.length then "end of file" else current

            // The design doc also uses "foo foo" / "bar bar" for
            // a first-level block; the slides use Foo / Bar.
            val useDesignDocAlias = depth == 1 && found == "foo foo"
            if !useDesignDocAlias && found != canonicalOpen then
                throw ParseFailure(
                    s"Expected '$canonicalOpen' block opener, found '$found'"
                )

            index += 1
            val close =
                if useDesignDocAlias then "bar bar"
                else "Bar" * depth
            Stmt.Block(parseStatements(Some(close), depth))

        private def parseIf(conditionSource: String, depth: Int): Stmt =
            val condition = parseExpression(conditionSource)
            val thenBranch = parseBlock(depth + 1)

            val elseBranch: Option[Stmt] =
                if index >= lines.length then None
                else
                    current match
                        case elseIf(otherCondition) =>
                            index += 1
                            Some(parseIf(otherCondition, depth))
                        case "foofoobarbar" =>
                            index += 1
                            Some(parseBlock(depth + 1))
                        case _ => None

            Stmt.If(condition, thenBranch, elseBranch)

        private def parseStatement(depth: Int): Stmt =
            val line = current
            index += 1

            line match
                case "FooBar()" =>
                    phase = RuntimeRules.nextPhase(phase)
                    Stmt.FooBar

                case assignment(name, source) =>
                    Stmt.Assign(name, parseExpression(source))

                case control(keyword, source) =>
                    keywordRole(keyword) match
                        case "if" => parseIf(source, depth)
                        case "while" =>
                            val condition = parseExpression(source)
                            Stmt.While(condition, parseBlock(depth + 1))
                        case _ =>
                            throw ParseFailure(
                                "Return is in the reference grammar but is not implemented yet"
                            )

                case other =>
                    throw ParseFailure(
                        s"Unsupported statement or syntax: '$other'"
                    )

        private def parseStatements(
            expectedClose: Option[String],
            depth: Int
        ): List[Stmt] =
            val statements = ArrayBuffer.empty[Stmt]

            while index < lines.length && !expectedClose.contains(current) do
                if Set("Bar", "BarBar", "BarBarBar", "bar bar").contains(current) then
                    throw ParseFailure(s"Unexpected block close: '$current'")
                statements += parseStatement(depth)

            expectedClose match
                case Some(close) => requireLine(close)
                case None => ()

            statements.toList

        def parseProgram(): Stmt =
            Stmt.Block(parseStatements(None, 0))
