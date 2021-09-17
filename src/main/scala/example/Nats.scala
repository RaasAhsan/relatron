package example

object Nats {
  
  import Core._

  enum Nat:
    case Z()
    case S(n: Nat)

  def zero: Term[Nat] = 
    Term.Value(())

  def succ(k: Term[Nat]): Term[Nat] =
    Term.Pair(k, Term.Value(()))

  given natReify: Reify[Nat] with
    def reify(term: Term[Nat], walk: [A] => Term[A] => Term[A]): Nat =
      walk(term) match {
        case Term.Value(()) => Nat.Z()
        case Term.Pair(t, _) => Nat.S(reify(t.asInstanceOf[Term[Nat]], walk))
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }

  def plus(a: Term[Nat], b: Term[Nat], c: Term[Nat]): Goal = {
    (a === zero && b === c) ||
      fresh[Nat, Nat] { (pa, pc) =>
        a === succ(pa) && c === succ(pc) && plus(pa, b, pc)
      }
    }

  // Constructive definition of nats... should we automatically infer this somehow?
  def nat(a: Term[Nat]): Goal =
    a === zero || fresh[Nat] { k =>
      a === succ(k) && nat(k)
    }

  def lte(a: Term[Nat], b: Term[Nat]): Goal =
    (a === zero && nat(b)) || 
      fresh[Nat, Nat] { (sa, sb) =>
        a === succ(sa) && b === succ(sb) && lte(sa, sb)
      }

}
