import scala.compiletime.ops.double
enum Expr:
    case Num(n: Int)
    case Bool(b: Boolean)
    case Var(name: String)
    case Plus(left: Expr, right: Expr)
    case Div(left: Expr, right: Expr)
    case Less(left: Expr, right: Expr)
    case If(test: Expr, yes: Expr, no: Expr)
    case Let(name: String, bound: Expr, body: Expr)
    case And(left: Expr, right: Expr)


enum Value:
    case IntV(n: Int)
    case BoolV(b: Boolean)

enum EvalError:
    case Unbound(name: String)
    case ExpectedInt
    case ExpectedBool
    case DivideByZero

import Expr.*, Value.*, EvalError.*


type Env = Map[String, Value]

def expect_int(left: Expr, right: Expr, env: Env = Map.empty): Either[EvalError, (Int, Int)] =
    val res1 = eval(left, env)
    val res2 = eval(right, env)
    (res1, res2) match
    case (Left(error), _) => Left(error)
    case (_, Left(error)) => Left(error)
    case (Right(x), Right(y)) =>
        (x, y) match
            case (BoolV(b), _) => Left(ExpectedInt)
            case (_, BoolV(b)) => Left(ExpectedInt)
            case (IntV(a), IntV(b)) => Right((a, b))


def eval(e: Expr, env: Env = Map.empty): Either[EvalError, Value] = 
    e match
        case Num(n) => Right(IntV(n))
        case Bool(b) => Right(BoolV(b))
        case Var(name) =>
            val x = env.get(name)
            x match
                case Some(value) => Right(value)
                case None => Left(Unbound(name))
            
        case Plus(e1, e2) => 
            expect_int(e1, e2, env) match
                case Left(error) => Left(error)
                case Right((a, b)) => Right(IntV(a+b))
            
        case Div(e1, e2) =>
            expect_int(e1, e2, env) match
                case Left(error) => Left(error)
                case Right((a, b)) => 
                    if b then
                        Right(IntV(a / b))
                    else
                        Left(DivideByZero)
            
        case Less(e1, e2) =>
            expect_int(e1, e2, env) match
                case Left(error) => Left(error)
                case Right((a, b)) => Right(BoolV(a < b))
            
        case If(test, yes, no) => 
            val res = eval(test, env)
            res match
                case Left(error) => Left(error)
                case Right(value) =>
                    value match
                        case IntV(x) =>
                            if x != 0 then
                                eval(yes, env)
                            else
                                eval(no, env)
                        case BoolV(b) =>
                            if b then
                                eval(yes, env)
                            else
                                eval(no, env)
        
        case Let(name, bound, body) =>
            val res = eval(bound, env)
            res match
                case Left(error) => Left(error)
                case Right(value) => 
                    val new_env = env + (name -> value)
                    eval(body, new_env)
            
        case And(left, right) => 
            // this is heinous
            val res1 = eval(left)
            res1 match
                case Left(error) => Left(error)
                case Right(value) => 
                    value match
                        case IntV(_) => Left(ExpectedBool)
                        case BoolV(b) => 
                            if b then
                                val res2 = eval(right)
                                res2 match
                                    case Left(error) => Left(error)
                                    case Right(value) => 
                                        value match
                                            case IntV(_) => Left(ExpectedBool)
                                            case BoolV(b) => 
                                                if b then
                                                    Right(BoolV(true))
                                                else
                                                    Right(BoolV(false))
                            else
                                Right(BoolV(false))            
                                
                    
            
    
@main def main() : Unit = 
    var expr = Let("x", Num(4),Plus(Let("x", Plus(Var("x"), Num(1)), Var("x")),Var("x")))
    val expr1 = Let("x", Num(10), Var("x"))

    val test1 = If(Less(Plus(Num(3), Num(2)), Div(Num(20), Num(2))), Bool(true), Bool(false)) // Checks if Plus, Div, Less, Num, Bool and If all work, returns BoolV
    val test2 = Let("x", Num(4),Plus(Let("x", Plus(Var("x"), Num(1)), Var("x")),Var("x"))) // Same test case as in the SO, checks if let works and if binding and shadowing are correct. Should be NumV(9)
    val test3 = Let("x", Num(4), If(And(Bool(false), Let("x", Val("x"), Less(Val("x"), Num(5)))), ))
    val test4 = ???
    val test5 = ???
    val test6 = ???

    var res = eval(expr)
    println(res)