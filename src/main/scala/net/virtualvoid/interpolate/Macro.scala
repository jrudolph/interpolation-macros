package net.virtualvoid.interpolate

import language.experimental.macros
import reflect.macros.Context
import util.control.NonFatal

object MacroImpl {
  def body(c: Context { type PrefixType = ATest.ACtx })(args: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._

    sealed trait Component
    case class LiteralStringComponent(text: String) extends Component
    case class ExpressionComponent(expr: c.Expr[Any]) extends Component

    def sourceOf(expr: c.Expr[_]): String = {
      val p = expr.tree.pos
      val source = new String(p.source.content)
      if (p.isRange) source.substring(p.start, p.end)
      else p.lineContent.substring(p.point - p.source.lineToOffset(p.source.offsetToLine(p.point)))
    }

    val Apply(_, List(Apply(_, stringExprs))) = c.prefix.tree
    val stringParts = (stringExprs: List[Tree]) map {
      case Literal(Constant(s: String)) => s
      case x@_ => println(x.getClass.getSimpleName+": "+x); "n/a"
    }

    def argStringExpr(arg: c.Expr[Any]): c.Expr[String] = {
      val source = c.literal(sourceOf(arg))
      reify(
        Console.BOLD + source.splice + Console.RESET +
          " [" + (try Console.GREEN + "= " + arg.splice.toString + Console.RESET catch {
            case NonFatal(x) =>
              s"${Console.RED}${x.getClass.getSimpleName}: ${x.getMessage}${Console.RESET}"
          }) + "]"
      )
    }
    def liftSeq[T](els: Seq[c.Expr[T]]): c.Expr[Seq[T]] =
      c.Expr[Seq[T]](Apply(Ident(definitions.List_apply), els.map(_.tree).toList))

    def showComponent(component: Component): c.Expr[String] = component match {
      case LiteralStringComponent(str) => c.literal(str)
      case ExpressionComponent(exp) => argStringExpr(exp)
    }
    def components: Seq[Component] =
      (stringParts.map(LiteralStringComponent), args.map(ExpressionComponent)).zipped.flatMap {
        case (a, b) => Seq(a, b)
      }

    val res: c.Expr[String] =
      reify(liftSeq(components.map(showComponent(_))).splice.mkString)

    // crashes without setting the outer position because of SI-6743
    c.Expr[String](atPos(c.prefix.tree.pos)(res.tree))
  }
}

object ATest {
  implicit class ACtx(val ctx: StringContext) extends AnyVal {
    def a(args: Any*): String = macro MacroImpl.body
  }
}
