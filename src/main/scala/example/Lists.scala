package example

object Lists {

  import Core._

  def nil[A]: Term[List[A]] =
    Term.Value(())

  def cons[A](head: Term[A], tail: Term[List[A]]): Term[List[A]] =
    Term.Pair(head, tail)

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
  
  given listReify[A](using RA: Reify[A]): Reify[List[A]] with
    def reify(term: Term[List[A]], walk: [A] => Term[A] => Term[A]): List[A] =
      walk(term) match {
        case Term.Value(()) => Nil
        case Term.Pair(h, t) => RA.reify(h.asInstanceOf[Term[A]], walk) :: reify(t.asInstanceOf[Term[List[A]]], walk)
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }
}
