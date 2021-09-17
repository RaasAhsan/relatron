package example
package interpreter

// Represents the evaluation of untyped arithmetic expressions which ranges over natural numbers and booleans
// TAPL chapter 3
object ArithmeticEvaluator {
  import Core._

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
    fresh[Node, Node, Node] { (t1, t2, t3) => n === test(t1, t2, t3) }

  given nodeReify: Reify[Node] with
    def reify(term: Term[Node], walk: [A] => Term[A] => Term[A]): Node =
      walk(term) match {
        case Term.Constructor("zero", _) => Zero
        case Term.Constructor("succ", t :: Nil) => Succ(reify(t.asInstanceOf[Term[Node]], walk))
        case Term.Constructor("pred", t :: Nil) => Pred(reify(t.asInstanceOf[Term[Node]], walk))
        case Term.Constructor("iszero", t :: Nil) => IsZero(reify(t.asInstanceOf[Term[Node]], walk))
        case Term.Constructor("true", _) => True
        case Term.Constructor("false", _) => False
        case Term.Constructor("if", t1 :: t2 :: t3 :: Nil) => If(reify(t1.asInstanceOf[Term[Node]], walk), reify(t2.asInstanceOf[Term[Node]], walk), reify(t3.asInstanceOf[Term[Node]], walk))
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }
      
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

  // one-step evaluation
  def eval(t1: Term[Node], t2: Term[Node]): Goal =
    (fresh[Node] { k => t1 === isZero(k) && k === zero && t2 === trueBool } ||
    fresh[Node, Node] { (k, t) => t1 === isZero(k) && k === succ(t) && t2 === falseBool } ||
    fresh[Node, Node] { (s2, s3) => t1 === test(trueBool, s2, s3) && t2 === s2 } ||
    fresh[Node, Node] { (s2, s3) => t1 === test(falseBool, s2, s3) && t2 === s3 } ||
    fresh[Node, Node, Node, Node] { (s1, s2, s3, s1e) => t1 === test(s1, s2, s3) && t2 === test(s1e, s2, s3) && eval(s1, s1e) }) && node(t1) && node(t2)

  // multi-step evaluation
  def multiEval(t1: Term[Node], t2: Term[Node]): Goal = 
    t1 === t2 || fresh[Node] { tk =>
      eval(t1, tk) && multiEval(tk, t2)
    }

}
