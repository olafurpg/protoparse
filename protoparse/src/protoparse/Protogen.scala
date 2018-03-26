package protoparse

import scala.meta._

import AST._
object Protogen {

  implicit class XtensionSeq[A](lst: Seq[A]) {
    def listMap[B](f: A => B): List[B] = lst.iterator.map(f).toList
    def listCollect[B](f: PartialFunction[A, B]): List[B] =
      lst.iterator.collect(f).toList
  }

  def toTermRef(ref: String): Term.Ref =
    ref.parse[Term].get.asInstanceOf[Term.Ref]
  val NoSelf = Self(Name.Anonymous(), None)
  val NoCtor = Ctor.Primary(Nil, Name.Anonymous(), Nil)

  def toStat(ast: AST): List[Stat] = ast match {
    case Enum(name, fields) =>
      val camelNames = fields.collect {
        case f: EnumField =>
          "is" + Case.snakeToCamel(f.name.toLowerCase()).capitalize
      }
      val init = Init(Type.Name(name), Name.Anonymous(), Nil)
      val isFieldAbstracts = camelNames.listMap { name =>
        q"def ${Term.Name(name)}: Boolean = false"
      }
      val typeName = Type.Name(name)
      val defnTrait = Defn.Trait(
        Mod.Sealed() :: Nil,
        typeName,
        Nil,
        NoCtor,
        Template(
          Nil,
          init"_root_.scalapb.GeneratedEnum" :: Nil,
          NoSelf,
          q"type EnumType = $typeName" :: isFieldAbstracts
        )
      )
      val values = {
        val args = fields.listCollect { case f: EnumField => Term.Name(f.name) }
        q"lazy val values: List[${Type.Name(name)}] = List(..$args)"
      }
      val unrecognized =
        q"case class Unrecognized(value: Int) extends $init with _root_.scalapb.UnrecognizedEnum"

      val stats = fields.iterator.zipWithIndex.map {
        case (f: EnumField, i) =>
          q"""case object ${Term.Name(f.name)} extends ${init} {
               val value = ${f.number}
               val index = $i
               val name = ${f.name}
               override def ${Term.Name(camelNames(i))} = true
             }"""
      }
      val fromValue = {
        val cases = fields.listCollect {
          case f: EnumField =>
            scala.meta.Case(Lit.Int(f.number), None, Term.Name(f.name))
        }
        val other = p"case __other => Unrecognized(__other)"
        q"def fromValue(value: Int): ${Type.Name(name)} = value match { ..case ${cases :+ other} }"
      }
      val companion = Defn.Object(
        Nil,
        Term.Name(name),
        Template(
          Nil,
          init"_root_.scalapb.GeneratedEnumCompanion[${Type.Name(name)}]" :: Nil,
          NoSelf,
          (stats ++ Iterator(values, fromValue)).toList)
      )
      defnTrait :: companion :: Nil
    case _ => sys.error(pprint.PPrinter.BlackWhite.apply(ast).toString())
  }
  def toScala(ast: List[AST]): Tree = ast match {
    case EmptyStatement :: tail => toScala(tail)
    case Package(ident) :: tail => Pkg(toTermRef(ident), tail.flatMap(toStat))
    case Nil => Source(Nil)
  }
}
