package AST {
  trait ASTNode {
    def to_viper(): String
  }

  trait ASTExpr extends ASTNode {}
}