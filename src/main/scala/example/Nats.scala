package example

object Nats {
  
  import Core._

  enum Nat:
    case Z()
    case S(n: Nat)

  import Nat._

  def zero: Term[Nat] = 
    Term.Constructor("zero", Nil)

  def succ(k: Term[Nat]): Term[Nat] =
    Term.Constructor("succ", List(k))

  given injectNat: Inject[Nat] with
    def inject(nat: Nat): Term[Nat] =
      nat match {
        case Z() => zero
        case S(k) => succ(inject(k))
      }

  given reifyNat: Reify[Nat] with
    def reify(term: Term[Nat]): Nat =
      term match {
        case Term.Constructor("zero", _) => Nat.Z()
        case Term.Constructor("succ", t :: Nil) => Nat.S(reify(t.asInstanceOf[Term[Nat]]))
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
