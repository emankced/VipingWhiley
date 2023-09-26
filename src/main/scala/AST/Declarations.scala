package AST {
  case class ASTIdent(name: String) extends ASTNode {
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
  case class ASTExpr(expr: String) extends ASTNode {
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

      left_side  + " = " + right_side + ";"
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

  case class ASTCodeBlock(indentation: Int, stmts: List[ASTNode]) extends ASTNode {
    override def to_viper(): String = {
      var res = "{"
      for(stmt <- stmts) {
        res += "\n" + " "*indentation + stmt.to_viper()
      }

      res + "\n}"
    }
  }
}