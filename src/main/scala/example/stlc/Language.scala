package example.stlc

trait Language {
  
  import example.stdlib.nat._

  enum Node:
    case Var(index: Nat)
    case Abs(t: Node)
    case App(t1: Node, t2: Node)

    

}
