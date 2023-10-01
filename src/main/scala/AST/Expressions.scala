package AST {
  case class ASTNullLiteral() extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = "null"
  }

  case class ASTBoolLiteral(value: Boolean) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = value.toString
  }

  case class ASTBinaryLiteral(value: Int) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = value.toString
  }

  case class ASTIntLiteral(value: Int) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = value.toString
  }

  case class ASTHexLiteral(value: Int) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = value.toHexString
  }

  case class ASTCharacterLiteral(value: String) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = "'" + value + "'"
  }

  case class ASTStringLiteral(value: String) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = "\"" + value + "\""
  }

  case class ASTIdent(name: String) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = name
  }

  // TODO handle actual expressions instead of accepting a string blindly
  case class ASTExprString(expr: String) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = expr
  }

  case class ASTInvokeExpr(name: ASTIdent, args: List[ASTExpr]) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var res = name.to_viper(adapt_for_function) + "("
      var first = true
      for(a <- args) {
        if(first) {
          res += a.to_viper(adapt_for_function)
          first = false
        } else {
          res += ", " + a.to_viper(adapt_for_function)
        }
      }

      res + ")"
    }
  }

  case class ASTUnaryOp(op: String, expr: ASTExpr) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = "-" + expr.to_viper(adapt_for_function)
  }

  case class ASTBinaryOp(expr0: ASTExpr, op: String, expr1: ASTExpr) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = expr0.to_viper(adapt_for_function) + " " + op + " " + expr1.to_viper(adapt_for_function)
  }

  case class ASTParanthesis(expr: ASTExpr) extends ASTExpr {
    override def to_viper(adapt_for_function: Boolean = false): String = "(" + expr.to_viper(adapt_for_function) + ")"
  }
}