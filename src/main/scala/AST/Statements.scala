package AST {
  case class ASTAssignStmt(lvals: List[ASTIdent], rvals: List[ASTExpr]) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var left_side = ""
      var first = true
      for(l <- lvals) {
        if(!first) {
          left_side += ", " + l.to_viper(adapt_for_function)
        } else {
          left_side += l.to_viper(adapt_for_function)
          first = false
        }
      }

      var right_side = ""
      first = true
      for(r <- rvals) {
        if(!first) {
          right_side += ", " + r.to_viper(adapt_for_function)
        } else {
          right_side += r.to_viper(adapt_for_function)
          first = false
        }
      }

      left_side  + " := " + right_side + ";"
    }
  }

  case class ASTVarDecl(lvals: List[(ASTType, ASTIdent)], rvals: List[ASTExpr]) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var decls = ""
      if(rvals.size > 0) {
        for(((t, i), e) <- lvals.zip(rvals)) {
          if(adapt_for_function) {
            decls += "let " + i.to_viper(adapt_for_function) + " == (" + e.to_viper(adapt_for_function) + ") in "
          } else {
            decls += "var " + i.to_viper(adapt_for_function) + ": " + t.to_viper(adapt_for_function) + " := " + e.to_viper(adapt_for_function) + "; "
          }
        }
      } else {
        for((t, i) <- lvals) {
          if(adapt_for_function) {
            System.err.println("Error: Forward declaration is not possible in functions!")
            System.exit(-1)
          } else {
            decls += "var " + i.to_viper(adapt_for_function) + ": " + t.to_viper(adapt_for_function) + "; "
          }
        }
      }

      decls
    }
  }

  case class ASTReturnStmt(exprs: List[ASTExpr]) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var res = ""
      if(!adapt_for_function) {
        res += "return"
      }

      var first = true
      for(e <- exprs) {
        if(first) {
          res += " " + e.to_viper(adapt_for_function)
          first = false
        } else {
          res += ", " + e.to_viper(adapt_for_function)
        }
      }

      res
    }
  }

  case class ASTControlStmt(name: String) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = name
  }

  case class ASTIfStmt(if_guard: ASTExpr, var code_block: ASTCodeBlock) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = "if(" + if_guard.to_viper(adapt_for_function) + ") " + code_block.to_viper(adapt_for_function)
  }

  case class ASTElseIfStmt(if_guard: ASTExpr, var code_block: ASTCodeBlock) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = "else { if(" + if_guard.to_viper(adapt_for_function) + ") " + code_block.to_viper(adapt_for_function) + " }"
  }

  case class ASTElseStmt(var code_block: ASTCodeBlock) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = "else " + code_block.to_viper(adapt_for_function)
  }

  case class ASTPrintStub(print_cmd: String) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = "// " + print_cmd
  }
}