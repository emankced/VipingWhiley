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
      value match {
        case Some(x) => "var " + ident.name + ": " + varType.typeName + " = " + x
        case _ => "var " + ident.name + ": " + varType.typeName
      }
    }
  }
}