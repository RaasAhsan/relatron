package example

// typed embedding of µkanren in scala
// programs are guaranteed to be well-formed
object Core extends Syntax {

  // tagged representation of terms
  enum Term[+T]:
    case Variable(index: Int)
    case Value(value: Any)
    case Constructor(name: String, terms: List[Term[Any]])

    def ===[U >: T](that: Term[U]): Goal =
      Goal.Unify(this, that)

  extension [T](t: T)
    def ===(that: Term[T]): Goal =
      Goal.Unify(Term.Value(t), that)

  def value[A](a: A): Term[A] =
    Term.Value[A](a)

  def int(i: Int): Term[Int] =
    value[Int](i)

  // goal constructors: unification, call/fresh, disj and conj
  enum Goal:
    case Fresh[T](unary: Term[T] => Goal)
    case Unify[T](left: Term[T], right: Term[T])
    case Conj(left: Goal, right: () => Goal)
    case Disj(left: Goal, right: () => Goal)

    def &&(that: => Goal): Goal = Conj(this, () => that)

    def ||(that: => Goal): Goal = Disj(this, () => that)

  def callFresh[T](unary: Term[T] => Goal): Goal =
    Goal.Fresh(unary)

  def succeed: Goal =
    () === Term.Value(())

  // triangular substitutions
  final case class State(variable: Int, subst: Map[Int, Term[_]]) { self =>
    def extend(index: Int, term: Term[_]): State = 
      copy(subst = subst + (index -> term))

    def newVariable: (State, Term[Nothing]) =
      copy(variable = variable + 1) -> Term.Variable(variable)

    def reify[T: Reify](index: Int): T =
      reify(subst.get(index).get.asInstanceOf[Term[T]])

    def reify[T](term: Term[T])(using RT: Reify[T]): T = {
      RT.reify(walk(this, term))
    }
  }

  trait Inject[T] {
    def inject(t: T): Term[T]
  }

  trait Reify[T] {
    def reify(term: Term[T]): T
  }

  def walk[T](state: State, term: Term[T]): Term[T] = {
    // println(s"walk: $state")
    term match {
      case Term.Variable(index) => 
        state.subst.get(index)
          .map(_.asInstanceOf[Term[T]])
          .map(t => walk(state, t))
          .getOrElse(term)
      case Term.Constructor(name, terms) =>
        Term.Constructor(name, terms.map(t => walk(state, t)))
      case _ => term
    }
  }

  def unify[T](state: State, t: Term[T], u: Term[T]): Option[State] = {
    // println("---")
    // println(state)
    // println(u)
    // println((walk(state, t), walk(state, u)))
    (walk(state, t), walk(state, u)) match {
      case (Term.Variable(tidx), Term.Variable(uidx)) if tidx == uidx => Some(state)
      case (Term.Variable(tidx), uwalk) => Some(state.extend(tidx, uwalk))
      case (twalk, Term.Variable(uidx)) => Some(state.extend(uidx, twalk))
      case (Term.Value(tvalue), Term.Value(uvalue)) if tvalue == uvalue => Some(state)
      case (Term.Constructor(n1, ts1), Term.Constructor(n2, ts2)) if n1 == n2 && ts1.length == ts2.length =>
        ts1.zip(ts2).foldLeft[Option[State]](Some(state)) { case (state, (t1, t2)) =>
          state.flatMap(s => unify(s, t1, t2))
        }
      case _ => None
    }
  }

  // interpreter for µkanren programs
  def run(goal: Goal): LazyList[State] = {
    def go(goal: Goal, state: State): LazyList[State] =
      goal match {
        case Goal.Fresh(unary) =>
          val (nextState, fresh) = state.newVariable
          go(unary(fresh), nextState)
        case Goal.Unify(l, r) => 
          LazyList.from(unify(state, l, r))
        case Goal.Conj(l, r) => 
          // bind
          // go(l, state).flatMap(nstate => go(r(), nstate))
          bind(go(l, state))(ns => go(r(), ns))
        case Goal.Disj(l, r) => 
          // mplus
          // go(l, state).lazyAppendedAll(go(r(), state))
          // complete search strategy: binary trampolining to interleave states from disjoint streams
          interleave(go(l, state), go(r(), state))
      }

    val init = State(0, Map())
    go(goal, init)
  }

  // Custom implementation of bind which interleaves
  def bind[A, B](left: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
    left match {
      case h #:: t => interleave(f(h), bind(t)(f))
      case LazyList() => LazyList()
    }

  def interleave[A](left: LazyList[A], right: => LazyList[A]): LazyList[A] = 
    left match {
      case h #:: t => h #:: interleave(right, t)
      case LazyList() => right
    }

  given injectInt: Inject[Int] with
    def inject(i: Int): Term[Int] =
      Term.Value(i)

  given reifyInt: Reify[Int] with
    def reify(term: Term[Int]): Int =
      term match {
        case Term.Value(x) => x.asInstanceOf[Int]
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }

  // TODO: can we abstract reification? so that we can reify recursive term references in any data structure

}