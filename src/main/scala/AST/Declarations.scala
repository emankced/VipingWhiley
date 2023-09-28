package AST {
  case class ASTIdent(name: String) extends ASTExpr {
    override def to_viper(): String = name
  }

  case class ASTType(typeName: String) extends ASTNode {
    override def to_viper(): String = {
      if(typeName.equals("int")) {
        "Int"
      } else if(typeName.equals("bool")) {
        "Bool"
      } else {
        typeName
      }
    }
  }

  // TODO handle actual expressions instead of accepting a string blindly
  case class ASTExprString(expr: String) extends ASTExpr {
    override def to_viper(): String = expr
  }

  case class ASTVariable(varType: ASTType, ident: ASTIdent) extends ASTNode {
    override def to_viper(): String = {
      val typeName = if (varType.typeName.equals("int")) {
        "Int"
      } else if (varType.typeName.equals("bool")) {
        "Bool"
      } else {
        varType.typeName
      }

      ident.to_viper() + ": " + typeName
    }

    def to_viper_as_result_type(): String = {
      val typeName = if (varType.typeName.equals("int")) {
        "Int"
      } else if (varType.typeName.equals("bool")) {
        "Bool"
      } else {
        varType.typeName
      }

      typeName
    }
  }
  case class ASTParameters(parameters: List[ASTVariable]) extends ASTNode {
    override def to_viper(): String = {
      var para: String = ""

      for(x <- parameters) {
        if(para.isEmpty) {
          para = x.to_viper()
        } else {
          para += ", " + x.to_viper()
        }
      }

      para
    }

    def to_viper_as_result_type(): String = {
      if(parameters.length > 1) {
        System.err.println("ASTParameters node has multiple return types!")
        System.exit(-1)
      }

      parameters.head.to_viper_as_result_type()
    }
  }

  case class ASTPackageDecl(name: String) extends ASTNode {
    override def to_viper(): String = "// package " + name
  }

  case class ASTImportDecl(import_string: String) extends ASTNode {
    override def to_viper(): String = "// " + import_string
  }

  case class ASTStaticVarDecl(varType: ASTType, ident: ASTIdent, value: Option[ASTExpr]) extends ASTNode {
    override def to_viper(): String = {
      //TODO cover: "null", "byte", "void", which are not supported by Viper
      value match {
        case Some(x) => "var " + ident.name + ": " + varType.to_viper() + " = " + x.to_viper()
        case _ => "var " + ident.name + ": " + varType.to_viper()
      }
    }
  }

  case class ASTFunctionDecl(ident: ASTIdent, parametersIn: ASTParameters, parametersOut: ASTParameters, requires: List[ASTExpr], ensures: List[ASTExpr], codeBlock: ASTCodeBlock) extends ASTNode {
    override def to_viper(): String = {
      var func = "function " + ident.to_viper() + "(" + parametersIn.to_viper() + "): " + parametersOut.to_viper_as_result_type()

      for(x <- requires) {
        func += "\n    requires " + x.to_viper()
      }

      for (x <- ensures) {
        func += "\n    ensures " + x.to_viper()
      }

      func + "\n" + codeBlock.to_viper()
    }
  }

  case class ASTMethodDecl(ident: ASTIdent, parametersIn: ASTParameters, parametersOut: ASTParameters, requires: List[ASTExpr], ensures: List[ASTExpr], codeBlock: ASTCodeBlock) extends ASTNode {
    override def to_viper(): String = {
      var func = "method " + ident.to_viper() + "(" + parametersIn.to_viper() + ") returns (" + parametersOut.to_viper() + ")"

      for(x <- requires) {
        func += "\n    requires " + x.to_viper()
      }

      for (x <- ensures) {
        func += "\n    ensures " + x.to_viper()
      }

      func + "\n" + codeBlock.to_viper()
    }
  }

  case class ASTAssignStmt(lvals: List[ASTIdent], rvals: List[ASTExpr]) extends ASTNode {
    override def to_viper(): String = {
      var left_side = ""
      var first = true
      for(l <- lvals) {
        if(!first) {
          left_side += ", " + l.to_viper()
        } else {
          left_side += l.to_viper()
          first = false
        }
      }

      var right_side = ""
      first = true
      for(r <- rvals) {
        if(!first) {
          right_side += ", " + r.to_viper()
        } else {
          right_side += r.to_viper()
          first = false
        }
      }

      left_side  + " := " + right_side + ";"
    }
  }

  case class ASTVarDecl(lvals: List[(ASTType, ASTIdent)], rvals: List[ASTExpr]) extends ASTNode {
    override def to_viper(): String = {
      var decls = ""
      if(rvals.size > 0) {
        for(((t, i), e) <- lvals.zip(rvals)) {
          // var i: Int := 0;
          decls += "var " + i.to_viper() + ": " + t.to_viper() + " := " + e.to_viper() + "; "
        }
      } else {
        for((t, i) <- lvals) {
          // var i: Int;
          decls += "var " + i.to_viper() + ": " + t.to_viper() + "; "
        }
      }

      decls
    }
  }

  case class ASTCodeBlock(var stmts: List[(Int, ASTNode)]) extends ASTNode {
    var block_indentation: Int = 0
    val block_indentation_step = 4
    override def to_viper(): String = {
      fix_block_indentation(0)

      var res = "{"
      for((indentation, stmt) <- stmts) {
        res += "\n" + " "*indentation + stmt.to_viper()
      }

      res + "\n" + " "*(block_indentation - block_indentation_step) + "}"
    }

    def fix_block_indentation(parent_block_indentation: Int): List[(Int, ASTNode)] = {
      if(parent_block_indentation < block_indentation) {
        return List()
      }

      block_indentation = parent_block_indentation + block_indentation_step

      var i = 0
      while(i < stmts.size) {
        val (ind, stmt) = stmts(i)
        if(ind > block_indentation) {
          System.err.println("Error: scope depth too high: " + ind + ">" + block_indentation)
          System.exit(-1)
        } else if(ind < block_indentation) {
          // if the indentation is smaller than expected, the code belongs to the parent code block
          val (a, b) = stmts.splitAt(i)
          stmts = a
          return b
        }

        // Handle code blocks in deeper nestings
        stmt match {
          case ASTIfStmt(_, cb) => {
            val ret = cb.fix_block_indentation(block_indentation);
              if(!ret.isEmpty) {
                val (a, b) = stmts.splitAt(i+1)
                stmts = a ++ ret ++ b
              }
            }
          case ASTElseIfStmt(_, cb) => {
            val ret = cb.fix_block_indentation(block_indentation);
              if(!ret.isEmpty) {
                val (a, b) = stmts.splitAt(i+1)
                stmts = a ++ ret ++ b
              }
            }
          case ASTElseStmt(cb) => {
            val ret = cb.fix_block_indentation(block_indentation);
              if(!ret.isEmpty) {
                val (a, b) = stmts.splitAt(i+1)
                stmts = a ++ ret ++ b
              }
            }
          case _ =>
        }

        i += 1
      }

      return List()
    }
  }

  case class ASTReturnStmt(exprs: List[ASTExpr]) extends ASTNode {
    override def to_viper(): String = {
      var res = "return"
      var first = true
      for(e <- exprs) {
        if(first) {
          res += " " + e.to_viper()
          first = false
        } else {
          res += ", " + e.to_viper()
        }
      }

      res
    }
  }

  case class ASTControlStmt(name: String) extends ASTNode {
    override def to_viper(): String = name
  }

  case class ASTIfStmt(if_guard: ASTExpr, var code_block: ASTCodeBlock) extends ASTNode {
    override def to_viper(): String = "if(" + if_guard.to_viper() + ") " + code_block.to_viper()
  }

  case class ASTElseIfStmt(if_guard: ASTExpr, var code_block: ASTCodeBlock) extends ASTNode {
    override def to_viper(): String = "else if(" + if_guard.to_viper() + ") " + code_block.to_viper()
  }

  case class ASTElseStmt(var code_block: ASTCodeBlock) extends ASTNode {
    override def to_viper(): String = "else " + code_block.to_viper()
  }

  case class ASTInvokeExpr(name: ASTIdent, args: List[ASTExpr]) extends ASTExpr {
    override def to_viper(): String = {
      var res = name.to_viper() + "("
      var first = true
      for(a <- args) {
        if(first) {
          res += a.to_viper()
          first = false
        } else {
          res += ", " + a.to_viper()
        }
      }

      res + ")"
    }
  }

  case class ASTUnaryOp(op: String, expr: ASTExpr) extends ASTExpr {
    override def to_viper(): String = "-" + expr.to_viper()
  }

  case class ASTBinaryOp(expr0: ASTExpr, op: String, expr1: ASTExpr) extends ASTExpr {
    override def to_viper(): String = expr0.to_viper() + " " + op + " " + expr1.to_viper()
  }

  case class ASTParenthised(expr: ASTExpr) extends ASTExpr {
    override def to_viper(): String = "(" + expr.to_viper() + ")"
  }
}