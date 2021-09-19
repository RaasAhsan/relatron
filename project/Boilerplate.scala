object Boilerplate {
  
  def fresh: String = {
    val methodDecls = (0 until 22).map { k =>
      val typeParams = (0 to k).map(i => s"A$i")
      val params = (0 to k).map(i => s"a$i")
      val typeParamsDecl = typeParams.mkString(", ")
      val fType = typeParams.map(t => s"Core.Term[$t]").mkString("(", ", ", ")") + " => Core.Goal"

      val inner = params.mkString("f(", ", ", ")")
      val nestings = (0 to k).foldRight(lines(line(inner))) { (i, acc) =>
        val typeParam = s"A$i"
        val param = s"a$i"
        lines(
          line(s"Core.callFresh[$typeParam] { $param => "),
          indent(acc),
          line("}")
        )
      }

      lines(
        line(s"def fresh[$typeParamsDecl](f: $fType): Core.Goal = "),
        indent(nestings),
        emptyLine
      )
    }

    lines(
      line("package example"),
      line("trait FreshBoilerplate {"),
      indent(methodDecls: _*),
      line("}")
    ).mkString
  }

  def run: String = {
    val methodDecls = (0 until 22).map { k =>
      val typeParams = (0 to k).map(i => s"A$i")
      val typeParamsWithBounds = typeParams.map(t => s"$t: Core.Reify")
      val params = (0 to k).map(i => s"a$i")
      val typeParamsDecl = typeParams.mkString(", ")
      val typeParamsWithBoundsDecl = typeParamsWithBounds.mkString(", ")
      val fType = typeParams.map(t => s"Core.Term[$t]").mkString("(", ", ", ")") + " => Core.Goal"

      val freshArguments = params.mkString(", ")
      val reifications = typeParams.zipWithIndex.map { case (tpe, idx) =>
        s"state.reify[$tpe]($idx)"
      }.mkString("(", ", ", ")")

      lines(
        line(s"def run[$typeParamsWithBoundsDecl](f: $fType): LazyList[($typeParamsDecl)] = {"),
        indent(
          line(s"val goal = fresh[$typeParamsDecl] { ($freshArguments) =>"),
          indent(line(s"f($freshArguments)")),
          line("}"),
          emptyLine,
          line("Core.run(goal).map { state =>"),
          indent(line(reifications)),
          line("}")
        ),
        line("}"),
        emptyLine
      )
    }

    lines(
      line("package example"),
      line("trait RunBoilerplate { self: FreshBoilerplate => "),
      indent(methodDecls: _*),
      line("}")
    ).mkString
  }

  case class Lines(lines: List[String]) {
    def mkString: String = lines.mkString("\n")
  }

  def line(s: String): Lines = 
    Lines(List(s))

  def emptyLine: Lines = Lines(List(""))

  def lines(lines: Lines*): Lines = 
    lines.foldLeft(Lines(List())) { (acc, lines) => Lines(acc.lines ++ lines.lines) }

  def indent(ls: Lines*): Lines = {
    val indented = ls.map(l => Lines(l.lines.map(s => s"  $s")))
    lines(indented: _*)
  }

}
