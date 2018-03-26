package protoparse

sealed trait AST extends Product with Serializable
object AST {
  trait Constant extends AST
  case class TermName(value: String) extends Constant
  case class LitInt(value: Int) extends Constant
  case class LitDouble(value: Double) extends Constant
  case class LitBool(value: Boolean) extends Constant
  case class LitString(value: String) extends Constant
  case object EmptyStatement extends AST
  case class Import(ident: String) extends AST
  case class Package(ident: String) extends AST
  case class Option(name: String, value: Constant) extends AST
  case class OneOfField(
      tpe: String,
      name: String,
      number: Int,
      options: Seq[Option])
      extends AST
  case class OneOf(name: String, fields: Seq[OneOfField]) extends AST
  case class MapField(
      keyTpe: String,
      valueTpe: String,
      name: String,
      number: Int,
      options: Seq[Option])
      extends AST
  case class Range(from: Int, to: scala.Option[Int]) extends AST
  case class EnumField(name: String, number: Int, options: Seq[Option])
      extends AST
  case class Enum(name: String, fields: Seq[AST]) extends AST
  case class Field(tpe: String, name: String, number: Int, fields: Seq[AST])
      extends AST
  case class Message(name: String, fields: Seq[AST]) extends AST
  case class Service(name: String, fields: Seq[AST]) extends AST
  case class RPC(
      name: String,
      inputTpe: String,
      outputTpe: String,
      options: Seq[Option])
      extends AST
  trait Reserved extends AST
  case class ReservedRanges(value: Seq[Range]) extends Reserved
  case class ReservedNames(value: Seq[String]) extends Reserved
}
