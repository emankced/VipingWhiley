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
}
