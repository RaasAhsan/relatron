package example

import Core._

@main
def main(): Unit = {
  import Lists.{given, *}
  // import Nats.{given, *}
  import arithmetic.{given, *}

  // val r1 = run[Nat] { x =>
  //   fresh[Nat, Nat] { (y, z) => 
  //     z === zero && y === succ(zero) && plus(z, x, y)
  //   }
  // }

  // println(r1.take(10).toList)

  // val r2 = run[List[Int], List[Int]] { (x, y) =>
  //   append(x, y, cons(int(1), cons(int(2), cons(int(3), cons(int(4), cons(int(5), nil))))))
  // }

  // val r2 = run[Nat] { x =>
  //   lte(zero, x)
  // }

  // val r2 = run[List[Int]] { x =>
  //   reverse(x, cons(int(1), cons(int(2), cons(int(3), nil))))
  // }

  try {
    // val r2 = run[Node] { x =>
    //   eval(x, falseBool)
    // }

    val r2 = run[Type] { x =>
      typing(test(falseBool, zero, succ(zero)), x)
    }
    println(r2.take(50).toList)
  } catch {
    case t =>
      t.printStackTrace()
  }
}
