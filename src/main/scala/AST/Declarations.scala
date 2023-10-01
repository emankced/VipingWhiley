package AST {
  case class ASTPrimitiveType(typeName: String) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      if(typeName.equals("int")) {
        "Int"
      } else if(typeName.equals("bool")) {
        "Bool"
      } else {
        typeName
      }
    }
  }

  case class ASTType(t: ASTNode, paranthesis: Boolean) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      if(paranthesis) {
        "(" + t.to_viper() + ")"
      } else {
        t.to_viper()
      }
    }
  }

  case class ASTVariable(varType: ASTType, ident: ASTIdent) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      ident.to_viper(adapt_for_function) + ": " + varType.to_viper()
    }

    def to_viper_as_result_type(): String = {
      varType.to_viper()
    }
  }
  case class ASTParameters(parameters: List[ASTVariable]) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var para: String = ""

      for(x <- parameters) {
        if(para.isEmpty) {
          para = x.to_viper(adapt_for_function)
        } else {
          para += ", " + x.to_viper(adapt_for_function)
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
    override def to_viper(adapt_for_function: Boolean = false): String = "// package " + name
  }

  case class ASTImportDecl(import_string: String) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = "// " + import_string
  }

  case class ASTStaticVarDecl(varType: ASTType, ident: ASTIdent, value: Option[ASTExpr]) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      //TODO cover: "null", "byte", "void", which are not supported by Viper
      value match {
        case Some(x) => "var " + ident.name + ": " + varType.to_viper(adapt_for_function) + " = " + x.to_viper(adapt_for_function)
        case _ => "var " + ident.name + ": " + varType.to_viper(adapt_for_function)
      }
    }
  }

  case class ASTFunctionDecl(ident: ASTIdent, parametersIn: ASTParameters, parametersOut: ASTParameters, requires: List[ASTExpr], ensures: List[ASTExpr], codeBlock: ASTCodeBlock) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var func = "function " + ident.to_viper(adapt_for_function) + "(" + parametersIn.to_viper(adapt_for_function) + "): " + parametersOut.to_viper_as_result_type()

      for(x <- requires) {
        func += "\n    requires " + x.to_viper(adapt_for_function)
      }

      for (x <- ensures) {
        func += "\n    ensures " + x.to_viper(adapt_for_function)
      }

      func + "\n" + codeBlock.to_viper(true)
    }
  }

  case class ASTMethodDecl(ident: ASTIdent, parametersIn: ASTParameters, parametersOut: ASTParameters, requires: List[ASTExpr], ensures: List[ASTExpr], codeBlock: ASTCodeBlock) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = {
      var func = "method " + ident.to_viper(adapt_for_function) + "(" + parametersIn.to_viper(adapt_for_function) + ") returns (" + parametersOut.to_viper(adapt_for_function) + ")"

      for(x <- requires) {
        func += "\n    requires " + x.to_viper(adapt_for_function)
      }

      for (x <- ensures) {
        func += "\n    ensures " + x.to_viper(adapt_for_function)
      }

      func + "\n" + codeBlock.to_viper(adapt_for_function)
    }
  }

  case class ASTCodeBlock(var stmts: List[(Int, ASTNode)]) extends ASTNode {
    var block_indentation: Int = 0
    val block_indentation_step = 4
    override def to_viper(adapt_for_function: Boolean = false): String = {
      fix_block_indentation(0)

      var res = "{"
      var i = 0
      while(i < stmts.size) {
        val (indentation, stmt) = stmts(i)
        if(adapt_for_function) {
          stmt match {
            case ASTIfStmt(if_guard, code_block) => {
              var cb = code_block.to_viper(adapt_for_function)
              cb = cb.splitAt(cb.size-1)._1
              cb = cb.splitAt(1)._2

              res += "\n" + " "*indentation + "(" + if_guard.to_viper(adapt_for_function) + " ? " + cb + " : "
              var closing_brackets_count = 1

              var break = false
              i = i+1
              while(!break && i < stmts.size) {
                val (next_indentation, next_stmt) = stmts(i)
                next_stmt match {
                  case ASTElseIfStmt(if_guard, code_block) => {
                    cb = code_block.to_viper(adapt_for_function)
                    cb = cb.splitAt(cb.size-1)._1
                    cb = cb.splitAt(1)._2
                    res += "(" + if_guard.to_viper(adapt_for_function) + " ? " + cb + " : "
                    closing_brackets_count += 1
                  }
                  case ASTElseStmt(code_block) => {
                    cb = code_block.to_viper(adapt_for_function)
                    cb = cb.splitAt(cb.size-1)._1
                    cb = cb.splitAt(1)._2
                    res += cb
                    break = true
                  }
                  case r: ASTReturnStmt => {
                    res += r.to_viper(adapt_for_function)
                    break = true
                  }
                  case n: ASTNode => res += n.to_viper()
                  case _ => break = true
                }
                i += 1
              }

              res += ")"*closing_brackets_count
            }
            case _ => res += "\n" + " "*indentation + stmt.to_viper(adapt_for_function)
          }
        } else {
          res += "\n" + " "*indentation + stmt.to_viper(adapt_for_function)
        }
        i += 1
      }

      res + "\n" + " "*(block_indentation - block_indentation_step) + "}"
    }

    def fix_block_indentation(parent_block_indentation: Int): List[(Int, ASTNode)] = {
      if(parent_block_indentation < block_indentation) {
        return List()
      }

      block_indentation = parent_block_indentation + block_indentation_step

      var i = 0
      while(i < stmts.size) {
        val (ind, stmt) = stmts(i)
        if(ind > block_indentation) {
          System.err.println("Error: scope depth too high: " + ind + ">" + block_indentation)
          System.exit(-1)
        } else if(ind < block_indentation) {
          // if the indentation is smaller than expected, the code belongs to the parent code block
          val (a, b) = stmts.splitAt(i)
          stmts = a
          return b
        }

        // Handle code blocks in deeper nestings
        stmt match {
          case ASTIfStmt(_, cb) => {
            val ret = cb.fix_block_indentation(block_indentation);
              if(!ret.isEmpty) {
                val (a, b) = stmts.splitAt(i+1)
                stmts = a ++ ret ++ b
              }
            }
          case ASTElseIfStmt(_, cb) => {
            val ret = cb.fix_block_indentation(block_indentation);
              if(!ret.isEmpty) {
                val (a, b) = stmts.splitAt(i+1)
                stmts = a ++ ret ++ b
              }
            }
          case ASTElseStmt(cb) => {
            val ret = cb.fix_block_indentation(block_indentation);
              if(!ret.isEmpty) {
                val (a, b) = stmts.splitAt(i+1)
                stmts = a ++ ret ++ b
              }
            }
          case _ =>
        }

        i += 1
      }

      return List()
    }
  }

  case class ASTModifier(modifier: String) extends ASTNode {
    override def to_viper(adapt_for_function: Boolean = false): String = "" // TODO check how modifiers work in viper
  }
}