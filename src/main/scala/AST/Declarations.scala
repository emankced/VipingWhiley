package AST {
  case class ASTIdent(name: String)

  case class ASTType(typeName: String)

  case class ASTPackageDecl(name: String)

  case class ASTImportDecl(import_string: String)

  case class ASTStaticVarDecl(varType: ASTType, ident: ASTIdent)
}