package AST {
  trait ASTNode {
    def to_viper(adapt_for_function: Boolean = false): String
  }

  trait ASTExpr extends ASTNode {}
}