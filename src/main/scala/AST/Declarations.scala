package AST {
  case class ASTIdent(name: String) extends ASTNode {
    override def to_viper(): String = name
  }

  case class ASTType(typeName: String) extends ASTNode {
    override def to_viper(): String = typeName
  }

  case class ASTPackageDecl(name: String) extends ASTNode {
    override def to_viper(): String = "// package " + name
  }

  case class ASTImportDecl(import_string: String) extends ASTNode {
    override def to_viper(): String = "// " + import_string
  }

  case class ASTStaticVarDecl(varType: ASTType, ident: ASTIdent, value: Option[String]) extends ASTNode { //TODO change Option[String] to Option[ASTExpr]
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
        case Some(x) => "var " + ident.name + ": " + typeName + " = " + x
        case _ => "var " + ident.name + ": " + typeName
      }
    }
  }
}