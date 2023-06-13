import cats.parse.Rfc5234.{alpha, char, cr, crlf, digit, htab, lf, sp}
import cats.parse.{Parser, Parser0}
import cats.parse.Parser.{not, char as pchar, charIn as pcharIn, string as pstring, stringIn as pstringIn}

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

    // No '\'' (0x27) and no '\\' (0x5c)
    val Character: Parser[String] = (pcharIn(0x01.toChar to 0x26.toChar) | pcharIn(0x28.toChar to 0x5b.toChar) | pcharIn(0x5d.toChar to 0x7f.toChar)).string
    val CharacterEscape: Parser[String] = (pcharIn('\\') ~ pcharIn('\\', 't', 'n', '\'')).string
    val CharacterLiteral: Parser[String] = pchar('\'') *> (Character | CharacterEscape).string <* pchar('\'')

    // No '"' (0x22) and no '\\' (0x5c)
    val StringCharacter: Parser[String] = (pcharIn(0x01.toChar to 0x21.toChar) | pcharIn(0x23.toChar to 0x5b.toChar) | pcharIn(0x5d.toChar to 0x7f.toChar)).string
    val StringEscape: Parser[String] = (pcharIn('\\') ~ pcharIn('\\', 't', 'n', '"')).string
    val StringLiteral: Parser[String] = pchar('"') *> (StringCharacter | StringEscape).rep.?.string <* pchar('"')

    val Literals: Parser[String] = NullLiteral | BoolLiteral | BinaryLiteral | IntLiteral | HexLiteral | CharacterLiteral | StringLiteral


    val x = StringLiteral.parse("\"\\nhey_kek lol \\\"\"")
    println(x);
  }
}

