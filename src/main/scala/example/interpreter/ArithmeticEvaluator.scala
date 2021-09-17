package example
package interpreter

// Encodes the evaluation of untyped arithmetic expressions which ranges over natural numbers and booleans
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
    ???

  // multi-step evaluation
  def multiEval(t1: Term[Node], t2: Term[Node]): Goal = 
    t1 === t2 || fresh[Node] { tk =>
      eval(tk, t2) && multiEval(t1, tk)
    }

}
