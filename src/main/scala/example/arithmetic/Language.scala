package example
package arithmetic

trait Language {

  import Core._

  // Represents the evaluation of untyped arithmetic expressions which ranges over natural numbers and booleans
  // TAPL chapter 3

  enum Node:
    case Zero
    case Succ(t: Node)
    case Pred(t: Node)
    case IsZero(t: Node)
    case True
    case False
    case If(t1: Node, t2: Node, t3: Node)

  private case object TermZeroTag extends ConstructorTag
  private case object TermSuccTag extends ConstructorTag
  private case object TermPredTag extends ConstructorTag
  private case object TermIsZeroTag extends ConstructorTag
  private case object TermTrueTag extends ConstructorTag
  private case object TermFalseTag extends ConstructorTag
  private case object TermIfTag extends ConstructorTag

  import Node._

  // TODO: try a pattern functor encoding, which may simplify injection of user-defined terms
  def zero: Term[Node] = Term.Constructor(TermZeroTag, Nil)

  def succ(t: Term[Node]): Term[Node] = Term.Constructor(TermSuccTag, List(t))

  def pred(t: Term[Node]): Term[Node] = Term.Constructor(TermPredTag, List(t))

  def isZero(t: Term[Node]): Term[Node] = Term.Constructor(TermIsZeroTag, List(t))

  def trueBool: Term[Node] = Term.Constructor(TermTrueTag, Nil)

  def falseBool: Term[Node] = Term.Constructor(TermFalseTag, Nil)

  def test(t1: Term[Node], t2: Term[Node], t3: Term[Node]): Term[Node] = Term.Constructor(TermIfTag, List(t1, t2, t3))

  def node(n: Term[Node]): Goal = 
    n === zero ||
    fresh[Node] { k => n === succ(k) && node(k) } || 
    fresh[Node] { k => n === pred(k) && node(k) } || 
    fresh[Node] { k => n === isZero(k) && node(k) } || 
    n === trueBool || 
    n === falseBool || 
    fresh[Node, Node, Node] { (t1, t2, t3) => n === test(t1, t2, t3) && node(t1) && node(t2) && node(t3) }

  def numeric(n: Term[Node]): Goal =
    n === zero || fresh[Node] { k => n === succ(k) && numeric(k) }

  given nodeReify: Reify[Node] with
    def reify(term: Term[Node]): Node =
      term match {
        case Term.Constructor(TermZeroTag, _) => Zero
        case Term.Constructor(TermSuccTag, t :: Nil) => Succ(reify(t.asInstanceOf[Term[Node]]))
        case Term.Constructor(TermPredTag, t :: Nil) => Pred(reify(t.asInstanceOf[Term[Node]]))
        case Term.Constructor(TermIsZeroTag, t :: Nil) => IsZero(reify(t.asInstanceOf[Term[Node]]))
        case Term.Constructor(TermTrueTag, _) => True
        case Term.Constructor(TermFalseTag, _) => False
        case Term.Constructor(TermIfTag, t1 :: t2 :: t3 :: Nil) => If(reify(t1.asInstanceOf[Term[Node]]), reify(t2.asInstanceOf[Term[Node]]), reify(t3.asInstanceOf[Term[Node]]))
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }
      
  enum Type:
    case Nat
    case Bool

  private case object TypeNatTag extends ConstructorTag
  private case object TypeBoolTag extends ConstructorTag

  def typeNat: Term[Type] = Term.Constructor(TypeNatTag, Nil)

  def typeBool: Term[Type] = Term.Constructor(TypeBoolTag, Nil)

  def typeNode(ty: Term[Type]): Goal = 
    ty === typeNat ||
    ty === typeBool

  given typeReify: Reify[Type] with
    def reify(term: Term[Type]): Type =
      term match {
        case Term.Constructor(TypeNatTag, _) => Type.Nat
        case Term.Constructor(TypeBoolTag, _) => Type.Bool
        case _ => throw new RuntimeException("invalid reification")
      }

  // Pattern functor encoding
  // enum NodeF[A]:
  //   case Zero()
  //   case Succ(t: A)
  //   case Pred(t: A)
  //   case IsZero(t: A)
  //   case True()
  //   case False()
  //   case If(t1: A, t2: A, t3: A)

  // final case class Node(t: NodeF[Node])
  // final case class NodeTerm(t: Term[NodeF[NodeTerm]])

}
