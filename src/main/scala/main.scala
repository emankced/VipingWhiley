
//import cats._
//import cats.data._
//import cats.syntax.all._;

object Main {
  def main(args: Array[String]): Unit = {
    println("// VipingWhiley")

    if(args.size != 1) {
      println("VipingWhiley is a transpiler to convert Whiley code to Viper code.")
      println()
      println("Usage: VipingWhiley <whiley-file>")
      println("\twhiley-file\tA file containing Whiley code")
      println()
      return
    }

    val sourceFile = args(0)
    val sourceCode = scala.io.Source.fromFile(sourceFile).mkString + "\n"
    WhileyParser.parse(sourceCode)
  }
}