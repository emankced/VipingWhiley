import cats.parse.Rfc5234.{lf, cr, crlf, sp, htab, alpha, digit, char}
import cats.parse.{Parser, Parser0}
import cats.parse.Parser.{char => pchar}
import cats.parse.Parser.{charIn => pcharIn}
import cats.parse.Parser.{string => pstring }
import cats.parse.Parser.{stringIn => pstringIn }

class WhileyParser() {
  def parse(): Unit = {
    val LineTerminator: Parser[Unit] = lf | cr | crlf
    val Indentation: Parser[String] = (htab | sp).rep.string

    // val _Letter: Parser[Char] = alpha
    val _Letter: Parser[Char] = pcharIn('_') | alpha
    // val Digit: Parser[Char] = digit

    //TODO comment parser

    // Identifiers
    val Ident: Parser[String] = _Letter.rep.string

    // Keywords
    val keyword_list = List("all", "any", "assert", "assume", "bool", "break", "byte",
      "continue", "else", "ensures", "export", "false", "finite", "for",
      "function", "if", "import", "in", "int", "is", "method", "native",
      "new", "null", "private", "protected", "public", "requires", "return",
      "skip", "total", "true", "void", "where", "while")
    /* Missing: case, catch, debug, default, do, fail, final, native,
                no, some, switch, throw, this, throws, try
     */
    val Keyword: Parser[String] = pstringIn(keyword_list)

    val keyword_identifier_list = List("constant")
    // Missing: from, type
    val KeywordIdentifier: Parser[String] = pstringIn(keyword_identifier_list)

    // Literals
    val NullLiteral: Parser[String] = pstringIn(List("null"))
    val BoolLiteral: Parser[String] = pstringIn(List("true", "false"))
    val BinaryLiteral: Parser[String] = pstring("0b") *> pcharIn('0', '1', '_').rep.string
    val IntLiteral: Parser[String] = digit.rep.string
    val HexLiteral: Parser[String] = pstring("0x") *> (digit | pcharIn('a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F', '_')).rep.string

    val Character: Parser[String] = char.string
    val CharacterEscape: Parser[String] = (pcharIn('\\') ~ pcharIn('\\', 't', 'n', '\'')).string
    val CharacterLiteral: Parser[String] = pchar('\'') *> Character | CharacterEscape <* pchar('\'')

    val StringEscape: Parser[String] = (pcharIn('\\') ~ pcharIn('\\', 't', 'n', '"')).string
    val StringLiteral: Parser[String] = pchar('"') *> (Character | StringEscape).rep.string <* pchar('"')

    val x = StringLiteral.parse("\"\"")

    val Literals: Parser[String] = NullLiteral | BoolLiteral | BinaryLiteral | IntLiteral | HexLiteral | CharacterLiteral | StringLiteral

    val useless: Parser[Unit] = LineTerminator | sp
    val p: Parser0[String] = useless.? *> _Letter.rep.string <* useless.?
    //val x = p.parse("\nhey_kek lol ")

    val p2: Parser[String] = digit.rep.string
    val y = p2.parse("42")

    println(x);
    println(y)
  }
}

