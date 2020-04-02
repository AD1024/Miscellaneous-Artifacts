package Data

package object AST {
    sealed abstract class CTerm
    sealed abstract class ITerm
    sealed abstract class Name
    sealed abstract class Type
    sealed abstract class Value
    sealed abstract class Neutral


    case class Ann(val term : CTerm, val ty : Type) extends ITerm
    case class Bound(val i : Int) extends ITerm
    case class Free(val name : Name) extends ITerm
    case class App(val func : ITerm, val arg : CTerm) extends ITerm

    case class Inf(val term : ITerm) extends CTerm
    case class Lam(val term : CTerm) extends CTerm

    case class Global(val name : String) extends Name
    case class Local(val i : Int) extends Name
    case class Quote(val i : Int) extends Name

    case class VLam(val func : Value => Value) extends Value
    case class VNeutral(val term : Neutral) extends Value

    case class NFree(val name : Name) extends Neutral
    case class NApp(val func : Neutral, val args : Value) extends Neutral

    def vfree(name : Name) = VNeutral(NFree(name))
}