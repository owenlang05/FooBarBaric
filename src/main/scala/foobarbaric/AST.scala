package foobarbaric

enum Expr:
    case Num(n: Int)
    case Bool(b: Boolean)
    case Var(name: String)
    case Plus(left: Expr, right: Expr)
    case Minus(left: Expr, right: Expr)
    case Mult(left: Expr, right: Expr)
    case Div(left: Expr, right: Expr)
    case Less(left: Expr, right: Expr)
    case And(left: Expr, right: Expr)
    case Call(name: String, args: List[Expr])

enum Value:
    case IntV(n: Int)
    case BoolV(b: Boolean)
