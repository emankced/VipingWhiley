package AST {
  case class ASTNullLiteral()

  case class ASTBoolLiteral(value: Boolean)

  case class ASTBinaryLiteral(value: Int)

  case class ASTIntLiteral(value: Int)

  case class ASTHexLiteral(value: Int)

  case class ASTCharacterLiteral(value: String)

  case class ASTStringLiteral(value: String)
}