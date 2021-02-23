
import scala.io._

class SyntaxException(gripe: String = "ERROR") extends Exception(gripe)
class MissingOperator extends SyntaxException("Missing operator!")
class NumberFormat extends SyntaxException("Illegal operand!")


object peano {


  def execute(cmmd: String): Boolean = {
    var thereIsOperator = false
    var allDigit = false
    var result = cmmd.replaceAll(" ", "")
    result = result.replaceAll("\\+", "")
    result = result.replaceAll("-", "")
    result = result.replaceAll("\\*", "")
    result = result.replaceAll("/", "")
    allDigit = result.forall(_.isDigit)
    if (!allDigit) {
      throw new Exception("there is letter and invalid character")
    }
    allDigit
  }


  // read-execute-print loop
  def repl {
    // repeatedly:
    var cmmd: String = ""
    var more = true
    //   1. prompt user for a string
    while(more){
      try {
        print("Please enter an expression =>")
        cmmd = StdIn.readLine
        if (cmmd == "quit") more = false
        else {
          val result = execute(cmmd)
          println(result)
        }
      }catch {
        case e: Exception => println(e)
      }
    }
    println("bye")
  }

  def main(args: Array[String]): Unit = {
    repl
  }
}
