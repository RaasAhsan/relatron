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

  test("stream interleaving stack safety") {
    val a = LazyList.continually(1)
    val b = LazyList.continually(2)

    val stream = interleave(a, b)
    val size = stream.take(30000).size

    assertEquals(size, 30000)
  }

  test("stream deep bind stack safety") {
    def go(n: Int): LazyList[Int] =
      if (n == 0) LazyList.empty[Int] else LazyList.apply(1).flatMap(_ => go(n - 1))

    val size = go(30000).take(1).size
    assertEquals(size, 1)
  }
}
