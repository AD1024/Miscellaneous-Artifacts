package lambdapi
import Data.AST._

package object Eval {
    type Env = List[Value]

    def vapp(f : Value, v : Value): Value = {
        f match {
            case VLam(func) => func(v)
            case VNeutral(term) => VNeutral(NApp(term, v))
        }
    }

    def evalITerm(term : ITerm, env : Env): Value = {
        term match {
            case Ann(term, ty) => evalCTerm(term, env)
            case App(func, arg) => vapp(evalITerm(func, env), evalCTerm(arg, env))
            case Bound(i) => env(i)
        }
    }

    def evalCTerm(term : CTerm, env : Env): Value = {
        term match {
            case Inf(t) => evalITerm(t, env)
            case Lam(term) => VLam(x => evalCTerm(term, x :: env))
        }
    }
}