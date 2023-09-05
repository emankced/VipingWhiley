import cats.parse.Rfc5234.{alpha, char, cr, crlf, digit, htab, lf, sp}
import cats.parse.{Parser, Parser0}
import cats.parse.Parser.{not, string0, char as pchar, charIn as pcharIn, string as pstring, stringIn as pstringIn}
import cats.implicits.toShow
import AST.*

class WhileyParser() {
  def parse(sourceCode: String): Unit = {
    val LineTerminator: Parser[Unit] = lf | cr | crlf
    val Indentation: Parser[String] = (htab | sp).rep.string

    // val _Letter: Parser[Char] = alpha
    val _Letter: Parser[Char] = pcharIn('_') | alpha
    // val Digit: Parser[Char] = digit

    //TODO comment parser

    // Identifiers
    val Ident: Parser[ASTIdent] = _Letter.rep.string.map(x => ASTIdent(x))

    /*
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
    */

    // Literals
    val NullLiteral: Parser[ASTNullLiteral] = pstringIn(List("null")).map(x => ASTNullLiteral())
    val BoolLiteral: Parser[ASTBoolLiteral] = pstringIn(List("true", "false")).string.map(x => ASTBoolLiteral(x.equals("true")))
    val BinaryLiteral: Parser[ASTBinaryLiteral] = pstring("0b") *> pcharIn('0', '1', '_').rep.string.map(bits => {
      var bin = 0
      for (x <- bits) {
        if (x == '1') {
          bin = bin << 1
          bin = bin | 1
        } else if (x == '0') {
          bin = bin << 1
        }
      }

      ASTBinaryLiteral(bin)
    })
    val IntLiteral: Parser[ASTIntLiteral] = digit.rep.string.map(x => ASTIntLiteral(x.toInt))
    val HexLiteral: Parser[ASTHexLiteral] = pstring("0x") *> (digit | pcharIn('a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F', '_')).rep.string.map(x => ASTHexLiteral(Integer.parseInt(x.replace("_", ""), 16)))

    // No '\'' (0x27) and no '\\' (0x5c)
    val Character: Parser[String] = (pcharIn(0x01.toChar to 0x26.toChar) | pcharIn(0x28.toChar to 0x5b.toChar) | pcharIn(0x5d.toChar to 0x7f.toChar)).string
    val CharacterEscape: Parser[String] = (pcharIn('\\') ~ pcharIn('\\', 't', 'n', '\'')).string
    val CharacterLiteral: Parser[ASTCharacterLiteral] = (pchar('\'') *> (Character | CharacterEscape).string <* pchar('\'')).map(x => ASTCharacterLiteral(x))

    // No '"' (0x22) and no '\\' (0x5c)
    val StringCharacter: Parser[String] = (pcharIn(0x01.toChar to 0x21.toChar) | pcharIn(0x23.toChar to 0x5b.toChar) | pcharIn(0x5d.toChar to 0x7f.toChar)).string
    val StringEscape: Parser[String] = (pcharIn('\\') ~ pcharIn('\\', 't', 'n', '"')).string
    val StringLiteral: Parser[ASTStringLiteral] = (pchar('"') *> (StringCharacter | StringEscape).rep0.string <* pchar('"')).map(x => ASTStringLiteral(x))

    val Literals: Parser[ASTNode] = NullLiteral | BoolLiteral | BinaryLiteral | HexLiteral | IntLiteral | CharacterLiteral | StringLiteral

    // Source files
    // PackageDecl rule
    val PackageDecl: Parser[ASTPackageDecl] = (pstring("package") ~ Indentation) *> (Ident ~ (pcharIn('.') ~ Ident).rep0).string.map(x => ASTPackageDecl(x))

    //TODO ImportDecl rule
    val FromSpec = (pcharIn('*') | Ident ~ (pchar(',') *> Indentation.? *> Ident).rep0) <* Indentation *> pstringIn(List("from")) <* Indentation
    val WithSpec = pstringIn(List("with")) <* Indentation *> (pcharIn('*') | (Ident ~ (pchar(',') *> Ident).rep0))
    val ImportDecl: Parser[ASTImportDecl] = (pstringIn(List("import")) <* Indentation *> FromSpec.backtrack.? ~ Ident ~ (pstringIn(List("::")) ~ (Ident | pcharIn('*').string)).rep0 ~ (Indentation *> WithSpec).?).string.map(x => ASTImportDecl(x))

    //val x = ImportDecl.parse("import yes")
    //val x = ImportDecl.parse("import * from a::pkg::File")
    //val x = ImportDecl.parseAll("import a::pkg::File with Oii")

    //TODO Expr rules

    // Type rules
    //TODO: For now only PrimitiveType is implemented. Missing: RecordType, ReferenceType, NominalType, ArrayType, FunctionType, MethodType
    /*
    val NullType = pstring("null")
    val BoolType = pstring("bool")
    val ByteType = pstring("byte")
    val IntType = pstring("int")
    val VoidType = pstring("void")
    */
    //TODO RealType is not properly specified in documentation...

    //TODO RealType missing!
    val PrimitiveType = pstringIn(List("null", "bool", "byte", "int", "void"))
    val Type: Parser[ASTType] = Parser.recursive[String] { recurse =>
      val TermType = (PrimitiveType | (pcharIn('(') <* Indentation.? *> recurse <* Indentation.? *> pcharIn(')'))).string

      //TODO fix UnionType
      //val UnionType = (TermType ~ (Indentation.?.void *> pcharIn('|').string <* Indentation.?.void *> TermType).rep).string

      Parser.oneOf(List(TermType))
    }.map(x => ASTType(x))

    // Expr rule
    val Expr: Parser[ASTExpr] = Parser.recursive[ASTExpr] { recurse =>
      //TODO Missing: CastExpr, LambdaExpr, ArrayExpr, RecordExpr, ReferenceExpr
      val ArithmeticNegationExpr = pchar('-') <* Indentation.? *> recurse
      val ArithmeticRelationalExpr = recurse <* Indentation.? *> pstringIn(List("<", "<=", "=>", ">")) <* Indentation.? *> recurse
      val ArithmeticAdditiveExpr = recurse <* Indentation.? *> pcharIn('+', '-') <* Indentation.? *> recurse
      val ArithmeticMultiplicativeExpr = recurse <* Indentation.? *>  pcharIn('*', '/', '%') <* Indentation.? *> recurse
      val ArithmeticExpr = ArithmeticNegationExpr | ArithmeticRelationalExpr | ArithmeticAdditiveExpr | ArithmeticMultiplicativeExpr

      /*
      val BitwiseExpr
      val EqualityExpr
      val InvokeExpr
      val LogicalExpr
      */
      val TermExpr = Ident | Literals | pcharIn('(') <* Indentation *> recurse <* Indentation *> pcharIn(')')

      //TODO properly parse expressions
      (TermExpr | ArithmeticExpr).string.map(x => ASTExpr(x))
    }

    //TODO TypeDecl rule
    //TODO StaticVarDecl rule
    val StaticVarDecl = (Type ~ (Indentation.void *> Ident) ~ (Indentation.void ~ pchar('=').void ~ Indentation.void *> Expr).?).map((x /*: Either[Parser.Error, (Option[ASTPackageDecl], List[Any])])*/ => ASTStaticVarDecl(x._1._1, x._1._2, x._2)))

    //TODO FunctionDecl rule
    val Variable = Type ~ (Indentation *> Ident)
    val Parameters = (Variable ~ (pchar(',') ~ Indentation.rep, Variable).rep0).?
    val FunctionDecl =  pstring("function") ~ Indentation *> Ident ~ (Indentation.rep0 ~ pchar('(') ~ Indentation.rep0 *> Parameters <* Indentation.rep0 ~ pchar(')') ~ Indentation.rep0 ~ pstring("->") ~ Indentation.rep0) ~ (pchar('(') ~ Indentation.rep0 *> Parameters <* Indentation.rep0 ~ pchar(')')) ~ (pstringIn(List("requires", "ensures")) ~ (Indentation *> Expr)).rep0

    //TODO MethodDecl rule

    //TODO Modifier rule
    val Modifier = pstringIn(List("public", "private", "native", "export", "final"))

    //TODO SourceFile rule
    val SourceFile = (LineTerminator.rep0 *> (PackageDecl <* LineTerminator.rep).? ~ ((ImportDecl <* LineTerminator) | ((Modifier <* Indentation).rep ~ (StaticVarDecl <* LineTerminator)) | (StaticVarDecl <* LineTerminator) | LineTerminator.void).rep0).map(x => {
      var root: List[ASTNode] = List()

      x._1 match {
        case Some(x) => root = root ++ List(x)
        case _ =>
      }

      for(node <- x._2) {
        node match {
          case x: ASTNode => root = root ++ List(x)
          case _ =>
        }
      }

      root
    })

    val x = SourceFile.parseAll(sourceCode)

    x match {
      case Left(error) => println(error.show)
      case Right(v) => {
        //println(v)
        for(node <- v) {
          println(node.to_viper())
        }
      }
    }
  }
}

