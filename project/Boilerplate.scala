object Boilerplate {
  
  // TODO: clean up nesting

  def fresh: String = {
    val methodDecls = (0 until 22).map { k =>
      val typeParams = (0 to k).map(i => s"A$i")
      val params = (0 to k).map(i => s"a$i")
      val typeParamsDecl = typeParams.mkString(", ")
      val fType = typeParams.map(t => s"Interpreter.Term[$t]").mkString("(", ", ", ")") + " => Interpreter.Goal"

      val inner = params.mkString("f(", ", ", ")")
      val nestings = (0 to k).foldRight(inner) { (i, acc) =>
        val typeParam = s"A$i"
        val param = s"a$i"
        s"""Interpreter.callFresh[$typeParam] { $param =>
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

  def run: String = {
    val methodDecls = (0 until 22).map { k =>
      val typeParams = (0 to k).map(i => s"A$i")
      val typeParamsWithBounds = typeParams.map(t => s"$t: Interpreter.Reify")
      val params = (0 to k).map(i => s"a$i")
      val typeParamsDecl = typeParams.mkString(", ")
      val typeParamsWithBoundsDecl = typeParamsWithBounds.mkString(", ")
      val fType = typeParams.map(t => s"Interpreter.Term[$t]").mkString("(", ", ", ")") + " => Interpreter.Goal"

      val freshArguments = params.mkString(", ")
      val reifications = typeParams.zipWithIndex.map { case (tpe, idx) =>
        s"state.reify[$tpe]($idx)"
      }.mkString("(", ", ", ")")

      s"""
        def run[$typeParamsWithBoundsDecl](f: $fType): LazyList[($typeParamsDecl)] = {
          val goal = fresh[$typeParamsDecl] { ($freshArguments) =>
            f($freshArguments)
          }

          Interpreter.run(goal).map { state =>
            $reifications
          }
        }
      """
    }.mkString("\n")

    s"""
      package example
      trait RunBoilerplate { self: FreshBoilerplate =>
        $methodDecls
      }
    """
  }

}
