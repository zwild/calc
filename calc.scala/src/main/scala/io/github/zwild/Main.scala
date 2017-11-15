package io.github.zwild

object Main {
  def main(arg: Array[String]): Unit = {
    def loop(): Unit = {
      print("> ")
      val in = scala.io.StdIn.readLine()
      in match {
        case ":q" => println("Bye.")
        case _ => CalcParsers(in) match {
          case Left(e) => println(e); loop()
          case Right(v) => Eval.eval(v) match {
            case Left(e) => println(e); loop()
            case Right(v) => println(v); loop()
          }
        }
      }
    }

    loop()
  }
}
