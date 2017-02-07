package model

import parser.Parsers
import parser.ReferenceTypes.Parser

import scala.language.implicitConversions
import scala.util.Try

trait DNA
object DNA {
  case object DNANull extends DNA
  case class DNAString(get: String) extends DNA
  case class DNAInteger(get: Int) extends DNA
  case class DNADouble(get: Double) extends DNA
  case class DNABool(get: Boolean) extends DNA
  case class DNALong(get: Long) extends DNA
  case class DNADateTime(get: String) extends DNA
  case class DNAArray(get: IndexedSeq[DNA]) extends DNA
  case class DNAObject(get: Map[String, DNA]) extends DNA

  def dnaParser[Parser[+_]](P: Parsers[Parser]): Parser[DNA] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}

    implicit def tok(s: String): Parser[String] = token(P.string(s))
    val dnaOrAnythingElse = regex("[a-zA-Z()_\"]* *\\{* *".r)

    def array: Parser[DNAArray] = surround(regex("\\[".r), regex("\\]".r))(
      value.sep(tok(",")).map(vs => DNAArray(vs.toIndexedSeq)).scope("array")
    )

    def obj: Parser[DNAObject] = {
      surround(dnaOrAnythingElse *> whitespace, whitespace *> tok("}") *> whitespace)(
        keyval.sep(tok(",")).map(kvs => DNAObject(kvs.toMap))
      ).scope("object")
    }

    def keyval: Parser[(String, DNA)] = escapedQuoted ** (regex("[a-zA-Z()_]*".r) *> whitespace *> (value | regex("[a-zA-Z()_]*".r).map(_ ⇒ DNANull)))

    def lit: Parser[DNA] = scope("literal") {
        tok("null").as(DNANull) |
        double.map(DNADouble(_)) |
//        tok("true").as(DNABool(true)) |
//        tok("false").as(DNABool(false)) |
        escapedQuoted.map {
          case b@("true" | "false") ⇒ DNABool(b.toBoolean)
          case "null" ⇒ DNANull
          case "" ⇒ DNANull
          case s ⇒ DNAString(s)
        }
    }

    def value: Parser[DNA] = lit | obj | array

    root(whitespace *> (obj | array))
  }

}

object Models {
  val open = "{"
  val close = "}"
}

trait ValueTypes
object ValueTypes {
  val Double = "Double"
  val String = "String"
  val Boolean = "Boolean"
  val Integer = "Integer"
  val Long = "Long"
  val DateTime = "DateTime"
  val DNA = "DNA"
  val BOArray = "BOArray"
  val Null = "null"
}

object GrammarPlusMore {
  val StringArray = "StringArray"
  val OpenBracket = "["
  val CloseBracket = "]"
  val ListSeparator = ","
  val OpenCloseQuote = "\""
  val Class = "class"
  val Blob = "blob"
}

/**
 * DNA parsing example.
 */
object JSONExample extends App {

  val dnaText =
    """DNA {
      |"someBool" Boolean "false"
      |}""".stripMargin


  val P = parser.Reference
  def printResult[E](e: Either[E, DNA]) =
    e.fold(println, println)

  val dna: Parser[DNA] = DNA.dnaParser(P)
  printResult { P.run(dna)(dnaText) }

}