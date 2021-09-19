package example

import munit.FunSuite

class CoreSuite extends FunSuite {

  import Core._

  test("stream interleaving") {
    val a = LazyList.continually(1)
    val b = LazyList.continually(2)

    val stream = interleave(a, b)
    val result = stream.take(4).toList

    assertEquals(result, List(1, 2, 1, 2))
  }
}
