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

    val keyword_list = List("all", "any", "assert", "assume", "bool", "break", "byte",
      "case", "catch", "continue", "debug", "default", "do", "else", "ensures", "export",
      "fail", "false", "final", "finite", "for",
      "function", "if", "import", "in", "int", "is", "method", "native",
      "new", "no", "null", "private", "protected", "public", "requires", "return",
      "skip", "some", "switch", "this", "throw", "throws", "total", "true", "try",
      "void", "where", "while")

    //TODO comment parser

    // Identifiers
    val Ident: Parser[ASTIdent] = _Letter.rep.string.map(x => {
      if(keyword_list.contains(x)) {
        System.err.println("Keywords may not be used as identifiers. Keyword used: " + x)
        //System.exit(-1)
      }

      ASTIdent(x)
    })

    /*
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

    val Literals: Parser[ASTExpr] = NullLiteral | BoolLiteral | BinaryLiteral | HexLiteral | IntLiteral | CharacterLiteral | StringLiteral

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
      val TermExpr = Ident | Literals | (pchar('(') ~ Indentation.? *> recurse <* Indentation.? ~ pchar(')')).backtrack.map(expr => ASTParenthised(expr))

      val InvokeExpr = ((Ident <* Indentation.?) ~ (pchar('(') ~ Indentation.? *> ((recurse <* Indentation.?) ~ (pchar(',') ~ Indentation.? *> recurse <* Indentation.?).rep0).? <* pchar(')'))).map(x => {
        val (name, arguments) = x
        val args = arguments match {
          case Some(a0, a_rest) => List(a0) ++ a_rest
          case _ => List()
        }

        ASTInvokeExpr(name, args)
      })

      //TODO Missing: CastExpr, LambdaExpr, ArrayExpr, RecordExpr, ReferenceExpr
      val NegationExpr = (pcharIn('-', '!', '~').string ~ (Indentation.? *> recurse)).map(x => {
        val (op, expr) = x
        ASTUnaryOp(op, expr)
      })
      val BinaryExpr = ((InvokeExpr.backtrack | TermExpr <* Indentation.?) ~ pstringIn(List("+", "-", "*", "/", "%", "<", "<=", ">=", ">", "==", "!=", "&", "|", "^", "<==>", "==>", "&&", "||")) ~ (Indentation.? *> recurse)).map(x => {
        val ((left, op), right) = x
        ASTBinaryOp(left, op, right)
      })

      val LogicalQuantExpr = (pstringIn(List("no", "some", "all")) ~ (Indentation.? ~ pchar('{') ~ Indentation.? *> (Ident <* Indentation ~ pstring("in") ~ Indentation) ~ (recurse <* Indentation.?) ~ (pchar(',') ~ Indentation.? *> (Ident <* Indentation ~ pstring("in") ~ Indentation) ~ (recurse <* Indentation.?)).rep0 | (recurse <* Indentation.?) <* pchar('}'))).string.map(x => ASTExprString(x))

      NegationExpr.backtrack | BinaryExpr.backtrack | LogicalQuantExpr.backtrack | InvokeExpr.backtrack | TermExpr
    }

    val testExprInput = "5 + 5"
    val testExpr = Expr.parseAll(testExprInput)
    testExpr match {
      case Left(error) => { System.err.println(error.show); System.exit(-2) }
      case Right(v) =>
    }


    //TODO TypeDecl rule
    // StaticVarDecl rule
    val StaticVarDecl = (Type ~ (Indentation *> Ident) ~ (Indentation.? ~ pchar('=') ~ Indentation.? *> Expr.backtrack).?).map(x => {
      val ((type_, ident), expr) = x
      ASTStaticVarDecl(type_, ident, expr)
    })

    val Variable = (Type ~ (Indentation *> Ident)).map(x => ASTVariable(x._1, x._2))
    val Parameters = (Variable ~ (Indentation.?.with1 ~ pchar(',') ~ Indentation.? *> Variable).rep0).?.map {
      case Some(x) => ASTParameters(List(x._1) ++ x._2)
      case _ => ASTParameters(List())
    }

    //TODO support empty lines
    val CodeBlock: Parser[ASTCodeBlock] = Parser.recursive[ASTCodeBlock] { recurse =>
      //TODO Statement
      // Missing: LVal.Ident | LVal[Expr] | *Expr
      val LVal = Ident
      val AssignStmt = ((LVal <* Indentation.?) ~ (pchar(',') ~ Indentation.? *> LVal <* Indentation.?).rep0 ~ (pchar('=') ~ Indentation.? *> Expr ~ (Indentation.?.with1 ~ pchar(',') ~ Indentation.? *> Expr).rep0)).map(x => {
        val ((lval0, lval_rest), (expr0, expr_rest)) = x
        ASTAssignStmt(List(lval0) ++ lval_rest, List(expr0) ++ expr_rest)
      })
      val testAssignStmtInput = "x,z=42+1337-yz,73"
      val testAssignStmt = AssignStmt.parseAll(testAssignStmtInput)
      testAssignStmt match {
        case Left(error) => { System.err.println(error.show); System.exit(-2) }
        case Right(v) =>
      }

      val VarDecl = ((Type <* Indentation) ~ Ident ~ (Indentation.?.with1 ~ pchar(',') *> Type ~ (Indentation *> Ident)).backtrack.rep0 ~ (Indentation.? ~ pchar('=') ~ Indentation.? *> Expr ~ (Indentation.?.with1 ~ pchar(',') ~ Indentation.? *> Expr).backtrack.rep0).?).map(x => {
        val (((type0, ident0), type_ident_rest), option_expr0_expr_rest) = x

        val type_ident = List((type0, ident0))++ type_ident_rest

        val exprs = option_expr0_expr_rest match {
          case Some((expr0, expr_rest)) => List(expr0) ++ expr_rest
          case _ => List()
        }

        ASTVarDecl(type_ident, exprs)
      })
      val testVarDeclInput = "int xz"
      val testVarDecl = VarDecl.parseAll(testVarDeclInput)
      testVarDecl match {
        case Left(error) => { System.err.println(error.show); System.exit(-2) }
        case Right(v) =>
      }

      val ReturnStmt = (pstring("return") *> (Indentation *> Expr ~ (Indentation.?.with1 ~ pchar(',') ~ Indentation.? *> Expr).rep0).?).map(x => {
        val exprs = x match {
          case Some(expr0, expr_rest) => List(expr0) ++ expr_rest
          case _ => List()
        }

        ASTReturnStmt(exprs)
      })
      val testReturnStmtInput = "return 5"
      val testReturnStmt = ReturnStmt.parseAll(testReturnStmtInput)
      testReturnStmt match {
        case Left(error) => { System.err.println(error.show); System.exit(-2) }
        case Right(v) =>
      }

      val ControlStmt = pstringIn(List("break", "continue", "skip")).map(x => ASTControlStmt(x))

      // If Statement
      val IfStmt = (pstring("if") ~ Indentation *> Expr ~ (Indentation.? ~ pchar(':') ~ Indentation.? *> recurse)).map(x => {
        val (if_guard, code_block) = x
        ASTIfStmt(if_guard, code_block)
      })

      val ElseIfStmt = (pstring("else") ~ Indentation ~ pstring("if") ~ Indentation *> Expr ~ (Indentation.? ~ pchar(':') ~ Indentation.? *> recurse)).map(x => {
        val (if_guard, code_block) = x
        ASTElseIfStmt(if_guard, code_block)
      })

      val ElseStmt = (pstring("else") ~ Indentation.? ~ pchar(':') ~ Indentation.? *> recurse).map(code_block => {
        ASTElseStmt(code_block)
      })

      val PrintStub = (pstringIn(List("io::println(", "io::print(")) ~ (StringLiteral | Ident).? ~ pcharIn(')')).string.map(x => ASTPrintStub(x))

      val Statement = PrintStub | ControlStmt | ReturnStmt | IfStmt | ElseIfStmt.backtrack | ElseStmt | VarDecl | AssignStmt

      (LineTerminator *> sp.rep.string ~ Statement).backtrack.rep.map(x => {
        val lines = x.toList.map((indentation, stmt) => (indentation.size, stmt))
        ASTCodeBlock(lines)
      })
    }

    // FunctionDecl rule
    val FunctionDecl =  (pstring("function") ~ Indentation *> Ident ~ (Indentation.? ~ pchar('(') ~ Indentation.? *> Parameters <* Indentation.? ~ pchar(')') ~ Indentation.? ~ pstring("->") ~ Indentation.?) ~ (pchar('(') ~ Indentation.? *> Parameters <* Indentation.? ~ pchar(')') ~ Indentation.?) ~ (LineTerminator.rep0.with1 *> pstringIn(List("requires", "ensures")) ~ (Indentation *> Expr.backtrack)).rep0 ~ (Indentation.? ~ pchar(':') ~ Indentation.? *> CodeBlock)).map(x => {
      val ((((ident, parametersIn), parametersOut), ensuresAndRequires), codeBlock) = x
      val ensures = ensuresAndRequires.filter((s, _) => s.equals("ensures")).map((_, node) => node)
      val requires = ensuresAndRequires.filter((s, _) => s.equals("requires")).map((_, node) => node)

      ASTFunctionDecl(ident, parametersIn, parametersOut, requires, ensures, codeBlock)
    })

    // MethodDecl rule
    val MethodDecl =  (pstring("method") ~ Indentation *> Ident ~ (Indentation.? ~ pchar('(') ~ Indentation.? *> Parameters <* Indentation.? ~ pchar(')') ~ Indentation.?) ~ (pstring("->") ~ Indentation.? ~ pchar('(') ~ Indentation.? *> Parameters <* Indentation.? ~ pchar(')') ~ Indentation.?).? ~ (LineTerminator.rep0.with1 *> pstringIn(List("requires", "ensures")) ~ (Indentation *> Expr.backtrack)).rep0 ~ (Indentation.? ~ pchar(':') ~ Indentation.? *> CodeBlock)).map(x => {
      val ((((ident, parametersIn), optParametersOut), ensuresAndRequires), codeBlock) = x
      val parametersOut = optParametersOut match {
        case Some(p) => p
        case _ => ASTParameters(List())
      }
      val ensures = ensuresAndRequires.filter((s, _) => s.equals("ensures")).map((_, node) => node)
      val requires = ensuresAndRequires.filter((s, _) => s.equals("requires")).map((_, node) => node)

      ASTMethodDecl(ident, parametersIn, parametersOut, requires, ensures, codeBlock)
    })

    // Modifier rule
    val Modifier = pstringIn(List("public", "private", "native", "export", "final")).map(x => ASTModifier(x))

    // SourceFile rule
    val SourceFile = (LineTerminator.rep0 *> (PackageDecl <* LineTerminator.rep).? ~ ((ImportDecl <* LineTerminator) | ((Modifier <* Indentation).rep0.with1 ~ (StaticVarDecl <* LineTerminator)).backtrack | ((Modifier <* Indentation).rep0.with1 ~ (FunctionDecl <* LineTerminator)).backtrack | ((Modifier <* Indentation).rep0.with1 ~ (MethodDecl <* LineTerminator)).backtrack | LineTerminator.void | (Indentation ~ LineTerminator).void).rep0).map(x => {
      var root: List[ASTNode] = List()

      val (optPackageDecl, rest) = x
      optPackageDecl match {
        case Some(x) => root = root ++ List(x)
        case _ =>
      }

      for(node <- rest) {
        node match {
          case x: ASTNode => root = root ++ List(x)
          case (l, s): (List[ASTModifier], ASTNode) => root = root ++ l ++ List(s)
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

