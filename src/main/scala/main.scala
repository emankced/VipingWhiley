
//import cats._
//import cats.data._
//import cats.syntax.all._;

@main
def main(sourceFile: String): Unit = {
  println("// VipingWhiley")
  val sourceCode = scala.io.Source.fromFile(sourceFile).mkString + "\n"
  val wp = WhileyParser();
  wp.parse(sourceCode)
}