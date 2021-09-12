object Boilerplate {
  
  def fresh: String = {
    val methodDecls = (1 until 22).map { k =>
      val typeParams = (0 to k).map(i => s"A$i")
      val params = (0 to k).map(i => s"a$i")
      val typeParamsDecl = typeParams.mkString(", ")
      val fType = typeParams.map(t => s"Interpreter.Term[$t]").mkString("(", ", ", ")") + " => Interpreter.Goal"

      val inner = params.mkString("f(", ", ", ")")
      val nestings = (0 to k).foldRight(inner) { (i, acc) =>
        val typeParam = s"A$i"
        val param = s"a$i"
        s"""Interpreter.fresh[$typeParam] { $param =>
            $acc
          }"""
      }
      
      s"""
        def fresh[$typeParamsDecl](f: $fType): Interpreter.Goal =
          $nestings
      """
    }.mkString("\n")

    s"""
      package example
      trait FreshBoilerplate {
        $methodDecls
      }
    """
  }

}
