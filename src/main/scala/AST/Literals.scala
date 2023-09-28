package AST {
  case class ASTNullLiteral() extends ASTExpr {
    override def to_viper(): String = "null"
  }

  case class ASTBoolLiteral(value: Boolean) extends ASTExpr {
    override def to_viper(): String = value.toString
  }

  case class ASTBinaryLiteral(value: Int) extends ASTExpr {
    override def to_viper(): String = value.toString
  }

  case class ASTIntLiteral(value: Int) extends ASTExpr {
    override def to_viper(): String = value.toString
  }

  case class ASTHexLiteral(value: Int) extends ASTExpr {
    override def to_viper(): String = value.toHexString
  }

  case class ASTCharacterLiteral(value: String) extends ASTExpr {
    override def to_viper(): String = "'" + value + "'"
  }

  case class ASTStringLiteral(value: String) extends ASTExpr {
    override def to_viper(): String = "\"" + value + "\""
  }
}