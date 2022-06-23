package potrait
import scala.quoted.*
import scala.annotation.experimental
import scala.language.reflectiveCalls

object Macroni:
  transparent inline def lol[T <: AnyKind]: String = lolVec[T].mkString("\n")

  inline def lolVec[T <: AnyKind]: Vector[String] = ${ lolMacro[T] }

  def lolMacro[T <: AnyKind: Type](using Quotes): Expr[Vector[String]] =
    Macroni[T].lolli

end Macroni

class Macroni[T <: AnyKind: Type](using q: Quotes):
  import q.reflect.*

  def lolli: Expr[Vector[String]] =
    val repr = lolRepr(TypeRepr.of[T]).split("\n").map(Expr(_))
    val args = Varargs(repr)

    '{ Vector($args: _*) }

  private def checkApps(params: List[String], apps: List[TypeRepr]): Boolean =
    apps.collect { case ParamRef(_, i) => i } == params.indices

  private def lolRepr(x: TypeRepr): String =
    (x.dealias.widen, x.classSymbol) match
      case (TypeRef(t, name), Some(sym)) =>
        s"""=========================
           |TypeRef $t $name
           |------------------------ 
           |
           |${lolSymbol(sym)}
           |========================""".stripMargin

      case (TypeLambda(params, _, AppliedType(inner, apps)), _) if checkApps(params, apps) =>
        s"""TypeLambda $params
             |
             |${lolRepr(inner)}""".stripMargin

      case _ =>
        s"Macroni $x"

  private def lolSymbol(s: Symbol): String =
    s"""
       | class def: ${s.isClassDef}
       | declarations
       |------------------------
       |${s.declarations.map(lolDeclaration).mkString("\n")}
       |------------------------
       |""".stripMargin

  private def lolDeclaration(s: Symbol): String =
    if s.isType
    then s" TYPE $s"
    else if s.isDefDef && s.name != "<init>"
    then lolDefDecl(s)
    else if s.isValDef
    then lolValDecl(s)
    else s.toString

  private def lolDefDecl(s: Symbol): String = s.tree match
    case DefDef(name, params, tpe, body) =>
      val res = tpe.tpe.show
      val sign = s.signature
      s""" DEF $s ${s.flags}
        | SIGNATURE: $sign
        | PARAMS :${lolParams(params)}
        | RESULT $res
        | FLAGS: ${getFlags(s.flags).mkString(",")}
        |""".stripMargin

  private def lolParams(params: List[ParamClause]): String =
    params.map(_.params.map(lolParam).mkString("\n  ", "\n  ", "")).mkString("\n  ~~~~~~~~~")

  private def lolParam(param: ValDef | TypeDef): String = param match
    case ValDef(name, tr, _) =>
      val t = tr.tpe.show
      s"$name : $t"
    case TypeDef(name, tr) =>
      tr match
        case tt: TypeTree =>
          val t = tt.tpe.show
          s"ANON $t"

  private def lolValDecl(s: Symbol): String =
    s""" VAL $s ${s.flags}
       | FLAGS: ${getFlags(s.flags).mkString(",")}
       |""".stripMargin

  private val allFlags =
    Flags.getClass.getDeclaredMethods.iterator
      .filter(_.getReturnType == classOf[Long])
      .map(f => f.getName -> f.invoke(Flags).asInstanceOf[Flags])
      .filter(_._2 != 0)
      .toMap

  private def getFlags(s: Flags) =
    for (name, flag) <- allFlags if s.is(flag) yield name
