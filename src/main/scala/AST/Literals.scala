package AST {
  case class ASTNullLiteral() extends ASTNode {
    override def to_viper(): String = "null"
  }

  case class ASTBoolLiteral(value: Boolean) extends ASTNode {
    override def to_viper(): String = value.toString
  }

  case class ASTBinaryLiteral(value: Int) extends ASTNode {
    override def to_viper(): String = value.toString
  }

  case class ASTIntLiteral(value: Int) extends ASTNode {
    override def to_viper(): String = value.toString
  }

  case class ASTHexLiteral(value: Int) extends ASTNode {
    override def to_viper(): String = value.toHexString
  }

  case class ASTCharacterLiteral(value: String) extends ASTNode {
    override def to_viper(): String = "'" + value + "'"
  }

  case class ASTStringLiteral(value: String) extends ASTNode {
    override def to_viper(): String = "\"" + value + "\""
  }
}