package example.arithmetic

trait Language {

  import example.Core._

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

  import Node._

  // TODO: try a pattern functor encoding, which may simplify injection of user-defined terms
  def zero: Term[Node] = Term.Constructor("zero", Nil)

  def succ(t: Term[Node]): Term[Node] = Term.Constructor("succ", List(t))

  def pred(t: Term[Node]): Term[Node] = Term.Constructor("pred", List(t))

  def isZero(t: Term[Node]): Term[Node] = Term.Constructor("iszero", List(t))

  def trueBool: Term[Node] = Term.Constructor("true", Nil)

  def falseBool: Term[Node] = Term.Constructor("false", Nil)

  def test(t1: Term[Node], t2: Term[Node], t3: Term[Node]): Term[Node] = Term.Constructor("if", List(t1, t2, t3))

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
        case Term.Constructor("zero", _) => Zero
        case Term.Constructor("succ", t :: Nil) => Succ(reify(t.asInstanceOf[Term[Node]]))
        case Term.Constructor("pred", t :: Nil) => Pred(reify(t.asInstanceOf[Term[Node]]))
        case Term.Constructor("iszero", t :: Nil) => IsZero(reify(t.asInstanceOf[Term[Node]]))
        case Term.Constructor("true", _) => True
        case Term.Constructor("false", _) => False
        case Term.Constructor("if", t1 :: t2 :: t3 :: Nil) => If(reify(t1.asInstanceOf[Term[Node]]), reify(t2.asInstanceOf[Term[Node]]), reify(t3.asInstanceOf[Term[Node]]))
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }
      
  enum Type:
    case Nat
    case Bool

  def typeNat: Term[Type] = Term.Constructor("type_nat", Nil)

  def typeBool: Term[Type] = Term.Constructor("type_bool", Nil)

  def typeNode(ty: Term[Type]): Goal = 
    ty === typeNat ||
    ty === typeBool

  given typeReify: Reify[Type] with
    def reify(term: Term[Type]): Type =
      term match {
        case Term.Constructor("type_nat", _) => Type.Nat
        case Term.Constructor("type_bool", _) => Type.Bool
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
