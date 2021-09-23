package example
package stdlib

trait Lists {

  import Core._

  private case object NilTag extends ConstructorTag
  private case object ConsTag extends ConstructorTag

  def nil[A]: Term[List[A]] =
    Term.Constructor(NilTag, Nil)

  def cons[A](head: Term[A], tail: Term[List[A]]): Term[List[A]] =
    Term.Constructor(ConsTag, List(head, tail))

  def append[A](x: Term[List[A]], y: Term[List[A]], xy: Term[List[A]]): Goal =
    (x === nil[A] && y === xy) ||
      fresh[A, List[A], List[A]] { (h, t, ty) =>
        x === cons(h, t) && xy === cons(h, ty) && append(t, y, ty)
      }

  def reverse[A](x: Term[List[A]], y: Term[List[A]]): Goal =
    (x === nil[A] && y === nil[A]) ||
      fresh[A, List[A]] { (h, t) =>
        x === cons(h, t) && fresh[List[A]] { at =>
          append(at, cons(h, nil), y) && reverse(t, at)
        }
      }

  given injectList[A](using RA: Inject[A]): Inject[List[A]] with
    def inject(list: List[A]): Term[List[A]] =
      list match {
        case Nil => nil[A]
        case h :: t => cons(RA.inject(h), inject(t))
      }

  given reifyList[A](using RA: Reify[A]): Reify[List[A]] with
    def reify(term: Term[List[A]]): List[A] =
      term match {
        case Term.Constructor(NilTag, _) => Nil
        case Term.Constructor(ConsTag, h :: t :: Nil) => RA.reify(h.asInstanceOf[Term[A]]) :: reify(t.asInstanceOf[Term[List[A]]])
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }
}
