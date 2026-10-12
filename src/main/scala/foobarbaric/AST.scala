package foobarbaric

// Expression structures
enum Expr:
    case Num(n: Int)
    case Bool(b: Boolean)
    case Var(name: String)

    // Arithmetic
    case Plus(left: Expr, right: Expr)
    case Div(left: Expr, right: Expr)
    case Minus(left: Expr, right: Expr)
    case Mult(left: Expr, right: Expr)

    // Comparison and logic
    case Less(left: Expr, right: Expr)
    case And(left: Expr, right: Expr)

// Runtime values
enum Value:
    case IntV(n: Int)
    case BoolV(b: Boolean)

