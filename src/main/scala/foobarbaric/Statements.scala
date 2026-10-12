package foobarbaric

enum Stmt:
    case Assign(name: String, expr: Expr)
    case Print(expr: Expr)
    case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
    case While(condition: Expr, body: Stmt)
    case Block(statements: List[Stmt])
    case FooBar
