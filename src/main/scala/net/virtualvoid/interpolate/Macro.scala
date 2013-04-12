package net.virtualvoid.interpolate

import language.experimental.macros
import reflect.macros.Context

object MacroImpl {
  def body(c: Context { type PrefixType = ATest.ACtx })(args: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._

    def sourceOf(expr: c.Expr[_]): String = {
      val p = expr.tree.pos
      val source = new String(p.source.content)
      if (p.isRange) source.substring(p.start, p.end)
      else p.lineContent.substring(p.point - p.source.lineToOffset(p.source.offsetToLine(p.point)))
    }

    val expr = c.literal(sourceOf(c.prefix))

    val first = args.head

    def argStringExpr(arg: c.Expr[Any]): c.Expr[String] = {
      val source = c.literal(sourceOf(arg))
      reify(source.splice + " = " + arg.splice.toString)
    }
    def liftSeq[T](els: Seq[c.Expr[T]]): c.Expr[Seq[T]] =
      c.Expr[Seq[T]](Apply(Ident(definitions.List_apply), els.map(_.tree).toList))

    //val Apply(_, args) = c.prefix.tree

    val argsString: c.Expr[String] =
      reify(liftSeq(args.map(argStringExpr(_))).splice.mkString("\n\n"))

    val res = reify(expr.splice + "\n\n"+ argsString.splice)
    // crashes without setting the outer position because of SI-6743
    c.Expr[String](atPos(c.prefix.tree.pos)(res.tree))
  }
}

object ATest {
  implicit class ACtx(val ctx: StringContext) extends AnyVal {
    def a(args: Any*): String = macro MacroImpl.body
  }
}
