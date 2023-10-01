import org.scalatest.funsuite.AnyFunSuite
import cats.parse.Parser
import AST.*

class TestWhileyParser extends AnyFunSuite {
  val wp = WhileyParser()
  
  def my_assert(x: Either[Parser.Error, ASTNode], expected: ASTNode): Unit = {
    assert(x match {
        case Left(e) => false
        case Right(expected) => true
        case _ => false
    })
  }

   def my_assert_neg(x: Either[Parser.Error, ASTNode]): Unit = {
    assert(x match {
        case Left(e) => true
        case _ => false
    })
  }

  test("Ident -> ASTIdent") {
    my_assert(wp.Ident.parseAll("test_Ident"), ASTIdent("test_Ident"))
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
    my_assert(wp.PackageDecl.parseAll("package this.is.a.test"), ASTPackageDecl("this.is.a.test"))
  }

  test("ImportDecl -> ASTImportDecl") {
    my_assert(wp.ImportDecl.parseAll("import this::is::a::test"), ASTImportDecl("import this::is::a::test"))
  }
}
