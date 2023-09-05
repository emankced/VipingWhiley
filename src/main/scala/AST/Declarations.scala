package AST {
  case class ASTIdent(name: String) extends ASTNode {
    override def to_viper(): String = name
  }

  case class ASTType(typeName: String) extends ASTNode {
    override def to_viper(): String = typeName
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
      val typeName = if(varType.typeName.equals("int")) {
        "Int"
      } else if(varType.typeName.equals("bool")) {
        "Bool"
      } else {
        varType.typeName
      }

      value match {
        case Some(x) => "var " + ident.name + ": " + typeName + " = " + x.to_viper()
        case _ => "var " + ident.name + ": " + typeName
      }
    }
  }

  case class ASTFunctionDecl(ident: ASTIdent, parametersIn: ASTParameters, parametersOut: ASTParameters, requires: List[ASTExpr], ensures: List[ASTExpr]) extends ASTNode {
    override def to_viper(): String = {
      var func = "function " + ident.to_viper() + "(" + parametersIn.to_viper() + "): " + parametersOut.to_viper_as_result_type()

      for(x <- requires) {
        func += "\n    requires " + x.to_viper()
      }

      for (x <- ensures) {
        func += "\n    ensures " + x.to_viper()
      }

      func + "\n"
    }
  }
}