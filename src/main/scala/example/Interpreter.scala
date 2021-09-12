package example

// typed embedding of µkanren in scala
// programs are guaranteed to be well-formed
object Interpreter extends FreshBoilerplate {

  // tagged representation of terms
  enum Term[+T]:
    case Variable(index: Int)
    case Value(value: Any)
    case Pair(left: Term[Any], right: Term[Any])

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

  def fresh[T](unary: Term[T] => Goal): Goal =
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

    def reify[T](term: Term[T])(using RT: Reify[T]): T =
      RT.reify(term, [A] => (tt: Term[A]) => walk[A](self, tt))
  }

  trait Reify[T] {
    def reify(term: Term[T], walk: [A] => Term[A] => Term[A]): T
  }

  def walk[T](state: State, term: Term[T]): Term[T] =
    term match {
      case Term.Variable(index) => 
        state.subst.get(index)
          .map(_.asInstanceOf[Term[T]])
          .map(t => walk(state, t))
          .getOrElse(term)
      case _ => term
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
      case (Term.Pair(ll, lr), Term.Pair(rl, rr)) =>
        unify(state, ll, rl).flatMap { nextState =>
          unify(nextState, lr, rr)
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
          go(l, state).flatMap(nstate => go(r(), nstate))
        case Goal.Disj(l, r) => 
          // mplus
          go(l, state).lazyAppendedAll(go(r(), state))
      }

    val init = State(0, Map())
    go(goal, init)
  }
  
  // Generate boilerplate with macros or source generators
  def run[A: Reify](f: Term[A] => Goal): LazyList[A] = {
    val goal = fresh[A] { a =>
      f(a)
    }

    run(goal).map { state =>
      state.reify[A](0)
    }
  }

  def run[A: Reify, B: Reify](f: (Term[A], Term[B]) => Goal): LazyList[(A, B)] = {
    val goal = fresh[A] { a =>
      fresh[B] { b =>
        f(a, b)
      }
    }

    run(goal).map { state =>
      state.reify[A](0) -> state.reify[B](1)
    }
  }

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

  def lte(a: Term[Nat], b: Term[Nat]): Goal =
    a === zero || 
      fresh[Nat, Nat] { (sa, sb) =>
        a === succ(sa) && b === succ(sb) && lte(sa, sb)
      }

  def nil[A]: Term[List[A]] =
    Term.Value(())

  def cons[A](head: Term[A], tail: Term[List[A]]): Term[List[A]] =
    Term.Pair(head, tail)

  def ::#[A](head: Term[A], tail: Term[List[A]]): Term[List[A]] =
    cons(head, tail)

  given intReify: Reify[Int] with
    def reify(term: Term[Int], walk: [A] => Term[A] => Term[A]): Int =
      walk(term) match {
        case Term.Value(x) => x.asInstanceOf[Int]
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }

  given listReify[A](using RA: Reify[A]): Reify[List[A]] with
    def reify(term: Term[List[A]], walk: [A] => Term[A] => Term[A]): List[A] =
      walk(term) match {
        case Term.Value(()) => Nil
        case Term.Pair(h, t) => RA.reify(h.asInstanceOf[Term[A]], walk) :: reify(t.asInstanceOf[Term[List[A]]], walk)
        case Term.Variable(_) => throw new RuntimeException("unbound variable")
        case _ => throw new RuntimeException("invalid reification")
      }

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

  def main(args: Array[String]): Unit = {
    // val r1 = run[Nat] { x =>
    //   fresh[Nat, Nat] { (y, z) => 
    //     z === zero && y === succ(zero) && plus(z, x, y)
    //   }
    // }

    // println(r1.take(10).toList)

    // val r2 = run[List[Int]] { x =>
    //   append(cons(int(1), cons(int(2), cons(int(3), nil))), x, cons(int(1), cons(int(2), cons(int(3), cons(int(4), cons(int(5), nil))))))
    // }

    // val r2 = run[Nat] { x =>
    //   lte(x, succ(succ(succ(succ(succ(succ(zero)))))))
    // }

    val r2 = run[List[Int]] { x =>
      reverse(x, cons(int(1), cons(int(2), cons(int(3), nil))))
    }

    println(r2.take(10).toList)
  }

  // TODO: can we abstract reification? so that we can reify recursive term references in any data structure

}