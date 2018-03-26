package protoparse

import fastparse.WhitespaceApi
import fastparse.core.Implicits.Repeater

object Protoparse {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(List(' ', '\n')).rep)
  }

  import fastparse.noApi._
  import White._
  val letter = P(CharIn(List('a' to 'z', 'A' to 'Z').flatten))
  val decimalDigit = P(CharIn('0' to '9'))
  val octalDigit = P(CharIn('0' to '7'))
  val hexDigit = P(CharIn(List('0' to '9', 'A' to 'F', 'a' to 'f').flatten))

  val ident = {
    import fastparse.all._
    P((letter ~ (letter | decimalDigit | "_").rep).!)
  }
  val fullIdent = P(ident.rep(sep = ".", min = 1).!)
  val messageName = P(ident)
  val enumName = P(ident)
  val fieldName = P(ident)
  val oneofName = P(ident)
  val mapName = P(ident)
  val serviceName = P(ident)
  val rpcName = P(ident)
  val messageType = P(messageName) //  ".".? ~ (ident ~ ".").rep ~ messageName
  val enumType = P(enumName) // ".".? ~ (ident ~ ".").rep ~ enumName

  val decimalLit = P(CharIn('1' to '9')) ~ decimalDigit.rep
  val octalLit = P("0" ~ octalDigit.rep)
  val hexLit = P("0" ~ ("x" | "X") ~ hexDigit.rep(1))
  val intLit = P(decimalLit | octalLit | hexLit)

  val decimals = P(decimalDigit ~ decimalDigit.rep)
  val exponent = P(("e" | "E") ~ P("+" | "-").? ~ decimals)
  val floatLit = P(
    decimals ~ "." ~ decimals.? ~ exponent.? | decimals ~ exponent | "." ~ decimals ~ exponent.?) | "inf" | "nan"

  val boolLit = P("true" | "false").!

  val quote = P(CharIn(List('\'', '"')))
  val hexEscape = P("\\" ~ ("x" | "X") ~ hexDigit ~ hexDigit)
  val octEscape = P("\\" ~ octalDigit ~ octalDigit ~ octalDigit)
  val charEscape = P(
    "\\" ~ P(CharIn(List('a', 'b', 'f', 'n', 'r', 't', 'v', '\\', ''', '''))))

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T) = f(t)
    override def toString() = name
  }
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
//  val charValue = P(
//    hexEscape | octEscape | charEscape |
//      CharPred(c => c != '\0' && c != '\n' && c != '\\'))

//  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(StringChars))

  val strLit = P("\"" ~/ (strChars | escape).rep.! ~ "\"")
//  val strLit = P(("'" ~ charValue.rep ~ "'") | ("\"" ~ charValue.rep ~ "\""))

  val emptyStatement = P(";")

  val constant = P(
    fullIdent | (P("-" | "+").? ~ intLit) | P("-" | "+").? ~ floatLit | strLit | boolLit)

  val syntax = P("syntax" ~ "=" ~ quote ~ "proto3" ~ quote ~ ";")

  val `import` = P("import" ~ ("weak" | "public").? ~ strLit ~ ";")

  val `package` = P("package" ~ fullIdent.! ~ ";")

  val optionName = P((ident | "(" ~ fullIdent ~ ")") ~ ("." ~ ident).rep)

  val option = P("option" ~ optionName ~ "=" ~ constant ~ ";")

  val `type` = P(
    "double" | "float" | "int32" | "int64" | "uint32" | "uint64" |
      "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" |
      "bool" | "string" | "bytes" | messageType | enumType)
  val fieldNumber = intLit;

  val fieldOption = P(optionName ~ "=" ~ constant)
  val fieldOptions = P(fieldOption ~ ("," ~ fieldOption).rep)
  val field = P(
    "repeated".? ~ `type` ~ fieldName ~ "=" ~ fieldNumber.! ~ ("[" ~ fieldOptions ~ "]").? ~ ";")

  val oneofField = P(
    `type` ~ fieldName ~ "=" ~ fieldNumber ~ ("[" ~ fieldOptions ~ "]").rep ~ ";")
  val oneof = P(
    "oneof" ~ oneofName ~ "{" ~ (oneofField | emptyStatement).rep ~ "}")

  val keyType = P(
    "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
      "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string")
  val mapField = P(
    "map" ~ "<" ~ keyType ~ "," ~ `type` ~ ">" ~ mapName ~ "=" ~ fieldNumber ~ ("[" ~ fieldOptions ~ "]").rep ~ ";")

  val range = intLit ~ ("to" ~ (intLit | "max")).?
  val ranges = range ~ ("," ~ range).rep
  val fieldNames = fieldName ~ ("," ~ fieldName).rep
  val reserved = P("reserved" ~ (ranges | fieldNames) ~ ";")

  val enumValueOption = P(optionName ~ "=" ~ constant)
  val enumField = P(
    ident ~ "=" ~ intLit ~ ("[" ~ enumValueOption
      .rep(sep = ",", min = 1)
      .! ~ "]").? ~ ";")
  val enumBody = P("{" ~ (option | enumField | emptyStatement).rep ~ "}")
  val enum = P("enum" ~ enumName ~ enumBody)

  val message = P("message" ~ messageName ~ messageBody)
  val messageBody: P[String] = P(
    "{" ~ (field | enum | message | option | oneof | mapField | reserved | emptyStatement).rep.! ~ "}")

  val service = P(
    "service" ~ serviceName ~ "{" ~ (option | rpc | emptyStatement).rep ~ "}")
  val rpc = P(
    "rpc" ~ rpcName ~ "(" ~ "stream".? ~ messageType ~ ")" ~ "returns" ~ "(" ~ "stream".? ~
      messageType ~ ")" ~ (("{" ~ (option | emptyStatement).rep ~ "}") | ";"))

  val topLevelDef = P(message | enum | service)
  val proto = P(
    syntax ~ (`import` | `package` | option | message | enum | service | emptyStatement)
      .rep(exactly = 4)
  )

  def main(args: Array[String]): Unit = {
    Repeater.UnitRepeater

    val example =
      """|syntax = "proto3";
         |import public "other.proto";
         |option java_package = "com.example.foo";
         |enum EnumAllowingAlias {
         |  option allow_alias = true;
         |  UNKNOWN = 0;
         |  STARTED = 1;
         |  RUNNING = 2 [(custom_option) = "hello world"];
         |}
         |message outer {
         |  option (my_option).a = true;
         |  message inner {
         |    int64 ival = 1;
         |  }
         |  repeated inner inner_message = 2;
         |  EnumAllowingAlias enum_field =3;
         |  map<int32, string> my_map = 4;
         |}
      """.stripMargin

    val e =
      """enum EnumAllowingAlias {
        |  option allow_alias = true;
        |  UNKNOWN = 0;
        |  STARTED = 1;
        |  RUNNING = 2 [(custom_option) = "hello world"];
        |}
      """.stripMargin

    val m =
      """message outer {
        |  option (my_option).a = true;
        |  message inner {
        |    int64 ival = 1;
        |  }
        |  inner inner_message = 2;
        |}
      """.stripMargin

    val i =
      """import public "other.proto";""".stripMargin

    val f = "repeated inner inner_message = 2;"

    val result =
//      ident.parse("a b = 2")
//      field.parse(f)
//    message.parse(m)
    proto.parse(example)

    pprint.log(result.toString)
  }
}
