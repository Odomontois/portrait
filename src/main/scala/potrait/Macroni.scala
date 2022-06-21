package potrait
import scala.quoted.*

object Macroni:
  transparent inline def lol[T <: AnyKind]: String = ${ lolMacro[T] }

  def lolMacro[T <: AnyKind: Type](using Quotes): Expr[String] =
    Macroni[T].lolli

end Macroni

class Macroni[T <: AnyKind: Type](using q: Quotes):
  import q.reflect.*

  def lolli: Expr[String] = Expr(lolRepr(TypeRepr.of[T]))

  private def lolRepr(x: TypeRepr): String =
    (x.dealias.widen, x.classSymbol) match
      case (TypeRef(t, name), Some(sym)) =>
        s"""TypeRef $t $name 
             |
             |${lolSymbol(sym)}""".stripMargin

      case (TypeLambda(params, bounds, AppliedType(inner, apps)), _) =>
        val recur = lolRepr(inner)

        s"""TypeLambda $params $apps
             |
             |$recur""".stripMargin

      case _ =>
        s"Macroni $x"

  private def lolSymbol(s: Symbol): String =
    s"""
       | ${s.isClassDef}
       | ${s.declarations}
       |""".stripMargin
