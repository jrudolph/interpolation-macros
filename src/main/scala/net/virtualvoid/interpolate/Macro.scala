package net.virtualvoid.interpolate

import language.experimental.macros
import reflect.macros.Context

object MacroImpl {
  def body(c: Context { type PrefixType = ATest.ACtx })(args: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._

    val p = c.prefix.tree.pos
    val source = new String(p.source.content)
    val exprS =
      if (p.isRange) source.substring(p.start, p.end)
      else p.lineContent.substring(p.point - p.source.lineToOffset(p.source.offsetToLine(p.point)))

    val expr = c.Expr[String](Literal(Constant(exprS)))
    val first = args.head

    reify(expr.splice + "\n\n> " + first.splice.toString)
  }
}

object ATest {
  implicit class ACtx(val ctx: StringContext) extends AnyVal {
    def a(args: Any*): String = macro MacroImpl.body
  }
}
