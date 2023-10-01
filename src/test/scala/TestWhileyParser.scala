import org.scalatest.funsuite.AnyFunSuite
import cats.parse.Parser
import AST.*

class TestWhileyParser extends AnyFunSuite {
  val wp = WhileyParser()
  
  def my_assert(x: Either[Parser.Error, ASTNode], expected: ASTNode): Unit = {
    assert(x match {
        case Left(e) => false
        case Right(actual) => actual == expected
        case _ => false
    })
  }

   def my_assert_neg(x: Either[Parser.Error, ASTNode]): Unit = {
    assert(x match {
        case Left(e) => true
        case _ => false
    })
  }

  test("Ident -> ASTIdent - 1") {
    my_assert(wp.Ident.parseAll("test_Ident"), ASTIdent("test_Ident"))
  }

  test("Ident -> ASTIdent - 2") {
    my_assert_neg(wp.Ident.parseAll("true"))
  }

  test("Ident -> Error - 1") {
    my_assert_neg(wp.Ident.parseAll("test_Ident2"))
  }

  test("Ident -> Error - 2") {
    my_assert_neg(wp.Ident.parseAll("test Ident"))
  }

  test("Literals -> ASTIntLiteral") {
    my_assert(wp.Literals.parseAll("1337"), ASTIntLiteral(1337))
  }

  test("Literals -> ASTHexLiteral") {
    my_assert(wp.Literals.parseAll("0xdead69"), ASTHexLiteral(0xdead69))
  }

  test("Literals -> ASTBoolLiteral") {
    my_assert(wp.Literals.parseAll("true"), ASTBoolLiteral(true))
  }

  test("PackageDecl -> ASTPackageDecl") {
    my_assert(wp.PackageDecl.parseAll("package lets.test.it"), ASTPackageDecl("lets.test.it"))
  }

  test("ImportDecl -> ASTImportDecl") {
    my_assert(wp.ImportDecl.parseAll("import lets::test::it"), ASTImportDecl("lets::test::it"))
  }

  test("Type -> ASTType - 1") {
    my_assert(wp.Type.parseAll("int"), ASTType(ASTPrimitiveType("int"), false))
  }

  test("Type -> ASTType - 2") {
    my_assert(wp.Type.parseAll("( int)"), ASTType(ASTPrimitiveType("int"), true))
  }

  test("Type -> ASTType - 3") {
    my_assert(wp.Type.parseAll("(( bool) )"), ASTType(ASTType(ASTPrimitiveType("bool"), true), true))
  }

  test("Expr -> ASTBinaryOp") {
    my_assert(wp.Expr.parseAll("5 + 5"), ASTBinaryOp(ASTIntLiteral(5), "+", ASTIntLiteral(5)))
  }

  test("Expr -> ASTUnaryOp") {
    my_assert(wp.Expr.parseAll("-3"), ASTUnaryOp("-", ASTIntLiteral(3)))
  }

  test("Expr -> Mixed Op") {
    my_assert(wp.Expr.parseAll("a + b * (-4)"), ASTBinaryOp(ASTIdent("a"), "+", ASTBinaryOp(ASTIdent("b"), "*", ASTParanthesis(ASTUnaryOp("-", ASTIntLiteral(4))))))
  }

  test("CodeBlock -> ASTCodeBlock") {
    my_assert(wp.CodeBlock.parseAll("\n    return 5"), ASTCodeBlock(List((4, ASTReturnStmt(List(ASTIntLiteral(5)))))))
  }

  test("CodeBlock -> IfTest") {
    my_assert(wp.CodeBlock.parseAll("\n    if 3 < 2:\n        x = 5"), ASTCodeBlock(List((4, ASTIfStmt(ASTBinaryOp(ASTIntLiteral(3), "<", ASTIntLiteral(2)), ASTCodeBlock(List((8, ASTAssignStmt(List(ASTIdent("x")), List(ASTIntLiteral(5)))))))))))
  }

  test("CodeBlock -> VarDecl") {
    my_assert(wp.CodeBlock.parseAll("\n    int x"), ASTCodeBlock(List((4, ASTVarDecl(List((ASTType(ASTPrimitiveType("int"), false), ASTIdent("x"))), List())))))
  }

  test("FunctionDecl -> ASTFunctionDecl") {
    my_assert(wp.FunctionDecl.parseAll("function test() -> (int result) requires 1 < 2:\n    return 3"), ASTFunctionDecl(ASTIdent("test"), ASTParameters(List()), ASTParameters(List(ASTVariable(ASTType(ASTPrimitiveType("int"), false), ASTIdent("result")))), List(ASTBinaryOp(ASTIntLiteral(1), "<", ASTIntLiteral(2))), List(), ASTCodeBlock(List((4, ASTReturnStmt(List(ASTIntLiteral(3))))))))
  }

  test("MethodDecl -> ASTMethodDecl") {
    my_assert(wp.MethodDecl.parseAll("method test():\n    int x = hey()"), ASTMethodDecl(ASTIdent("test"), ASTParameters(List()), ASTParameters(List()), List(), List(), ASTCodeBlock(List((4, ASTVarDecl(List((ASTType(ASTPrimitiveType("int"), false), ASTIdent("x"))), List(ASTInvokeExpr(ASTIdent("hey"), List()))))))))
  }
}
