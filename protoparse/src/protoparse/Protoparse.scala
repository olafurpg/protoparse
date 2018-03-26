package protoparse

import fastparse.WhitespaceApi
import fastparse.core.Implicits.Repeater

object Protoparse {
  private object Basic {
    import fastparse.all._
    val Newline = P(StringIn("\r\n", "\n"))
    val Whitespace = P(CharIn(List(' ', '\n')))
    val LineComment = {
      val SameLineCharChunks = P(CharsWhile(c => c != '\n' && c != '\r'))
      P("//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End))
    }
  }

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace((Basic.Whitespace | Basic.LineComment).rep)
  }
  import AST._

  private object Single {
    import fastparse.all._
    val letter = P(CharIn(List('a' to 'z', 'A' to 'Z').flatten)).!
    val decimalDigit = P(CharIn('0' to '9')).!
    val octalDigit = P(CharIn('0' to '7'))
    val hexDigit = P(CharIn(List('0' to '9', 'A' to 'F', 'a' to 'f').flatten)).!
    import fastparse.all._ // shadow `import White._`
    val ident = P((letter ~ (letter | decimalDigit | "_").rep).!)
    val fullIdent = P(ident.rep(sep = ".", min = 1)).!
    val messageName = P(ident)
    val enumName = P(ident)
    val fieldName = P(ident)
    val oneofName = P(ident)
    val mapName = P(ident)
    val serviceName = P(ident)
    val rpcName = P(ident)
    val messageType = P(messageName) //  ".".? ~ (ident ~ ".").rep ~ messageName
    val enumType = P(enumName) // ".".? ~ (ident ~ ".").rep ~ enumName

    val decimalLit = decimalDigit.rep(1).!.map(_.toInt)
    val octalLit = P("0" ~ octalDigit.rep).!.map(o => Integer.parseInt(o, 16))
    val hexLit: P[Int] =
      P("0" ~ ("x" | "X") ~ hexDigit.rep(1).!).map(h => Integer.parseInt(h, 16))
    val intLit: P[Int] = P(hexLit | octalLit | decimalLit)
    val decimals = P(decimalDigit.rep(1)).map(_.mkString.toInt)
    val exponent = P(("e" | "E") ~ P("+" | "-").? ~ decimals)
    val infinity: P[Double] = P("inf").map(_ => Double.PositiveInfinity)
    val nan: P[Double] = P("nan").map(_ => Double.NaN)
    val floatLit: P[Double] = P(
//      (decimals ~ "." ~ decimals.? ~ exponent.? | decimals ~ exponent | "." ~ decimals ~ exponent.?) |
      decimals.map(_.toDouble) | infinity | nan
    )

    val boolLit: P[Boolean] = P("true" | "false").!.map {
      case "true" => true
      case "false" => false
    }

    val quote = P(CharIn(List('\'', '"')))
    val hexEscape =
      P("\\" ~ ("x" | "X") ~ hexDigit ~ hexDigit).!
//        .map { case (a, b) => Integer.parseInt(a + b, 16) }
    val octEscape =
      P("\\" ~ octalDigit ~ octalDigit ~ octalDigit).!
//        .map { case (a, b, c) => Integer.parseInt(a + b + c, 8) }
    val charEscape = P("\\" ~ P(
      CharIn(List('a', 'b', 'f', 'n', 'r', 't', 'v', '\\', ''', ''')))).!
    val charValue: P[String] = P(
      hexEscape | octEscape | charEscape |
        CharPred(c => c != '\0' && c != '\n' && c != '\\').!)

    val strLit: P[String] =
      P(("'" ~ charValue.rep.! ~ "'") | ("\"" ~ charValue.rep.! ~ "\""))
        .map(_.mkString)
    val emptyStatement = P(";").map(_ => EmptyStatement)

    val constant: P[Constant] = P(
      fullIdent.map(TermName.apply) |
        (("-" | "+").? ~ intLit.map(LitInt.apply)) | // TODO negative
        (("-" | "+").? ~ floatLit.map(LitDouble.apply)) |
        strLit.map(LitString.apply) |
        boolLit.map(LitBool.apply)
    )

  }

  import Single._
  import fastparse.noApi._
  import White._

  val syntax: P[AST] =
    P("syntax" ~ "=" ~ quote ~ "proto3" ~ quote ~ ";").map(_ => EmptyStatement)

  val `import` =
    P("import" ~ ("weak" | "public").? ~ strLit.! ~ ";").map(Import.apply)

  val `package` = P("package" ~ fullIdent.! ~ ";").map(Package.apply)

  val optionName: P[String] =
    P((ident.! | "(" ~ fullIdent.! ~ ")").! ~ ("." ~ ident).rep.!).map {
      case (a, b) => a + b // ???
    }

  val option =
    P("option" ~ optionName ~ "=" ~ constant ~ ";").map(Option.tupled)

  val `type` = P(
    "double" | "float" | "int32" | "int64" | "uint32" | "uint64" |
      "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" |
      "bool" | "string" | "bytes" | messageType | enumType).!
  val fieldNumber = intLit

  val fieldOption = P(optionName ~ "=" ~ constant).map(Option.tupled)
  val fieldOptions: P[Seq[Option]] =
    P("[" ~ fieldOption.rep(sep = ",", min = 1) ~ "]").?.map(_.getOrElse(Nil))
  val field: P[Field] = P(
    "repeated".? ~ `type` ~ fieldName ~ "=" ~ fieldNumber ~ fieldOptions ~ ";")
    .map(Field.tupled)

  val oneofField = P(
    `type` ~ fieldName ~ "=" ~ fieldNumber ~
      ("[" ~ fieldOptions ~ "]").?.map(_.getOrElse(Nil)) ~ ";")
    .map(OneOfField.tupled)
  val oneof = P(
    "oneof" ~ oneofName ~ "{" ~
      (oneofField | emptyStatement).rep.map(_.collect {
        case o: OneOfField => o
      }) ~ "}").map(OneOf.tupled)

  val keyType = P(
    "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
      "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string").!

  val mapField = P(
    "map" ~ "<" ~ keyType ~ "," ~ `type` ~ ">" ~ mapName ~ "=" ~ fieldNumber ~ fieldOptions ~ ";")
    .map(MapField.tupled)

  val max: P[Int] = "max".!.map(_ => Int.MaxValue)

  val range = (intLit ~ ("to" ~ (intLit | max)).?).map(Range.tupled)
  val ranges: P[Reserved] =
    range.rep(sep = ",", min = 1).map(ReservedRanges.apply)
  val fieldNames: P[Reserved] =
    fieldName.rep(sep = ",", min = 1).map(ReservedNames.apply)
  val reserved: P[Reserved] = P("reserved" ~ (ranges | fieldNames) ~ ";")

  val enumValueOption = P(optionName ~ "=" ~ constant).map(Option.tupled)
  val enumValueOptions =
    ("[" ~ enumValueOption.rep(sep = ",", min = 1) ~ "]").?.map(
      _.getOrElse(Nil))
  val enumField =
    P(ident ~ "=" ~ intLit ~ enumValueOptions ~ ";").map(EnumField.tupled)
  val enumBody = P("{" ~ (option | enumField | emptyStatement).rep ~ "}")
  val enum: P[Enum] = P("enum" ~ enumName ~ enumBody).map(Enum.tupled)

  val message = P("message" ~ messageName ~ messageBody).map(Message.tupled)
  val messageBody: P[Seq[AST]] = P(
    "{" ~ (field | enum | message | option | oneof | mapField | reserved | emptyStatement).rep ~ "}")

  val service =
    P("service" ~ serviceName ~ "{" ~ (option | rpc | emptyStatement).rep ~ "}")
      .map(Service.tupled)
  val rpcOptions: P[Seq[Option]] = P(
    "{" ~ (option | emptyStatement).rep.map(_.collect {
      case o: Option => o
    }) ~ "}"
  )
  val rpc: P[AST] = P(
    "rpc" ~ rpcName ~ "(" ~ "stream".? ~ messageType ~ ")" ~ "returns" ~ "(" ~ "stream".? ~
      messageType ~ ")" ~ (rpcOptions | ";".!.map(_ => Nil))).map(RPC.tupled)

  val topLevelDef = P(message | enum | service)
  val proto: P[Seq[AST]] = P(
    syntax ~ (`import` | `package` | option | topLevelDef | emptyStatement).rep ~ End
  ).map(_._2)
}
