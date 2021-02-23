import scala.util._
import util.control.Breaks._

object BlackJack2 extends App {

  val gen = new Random(System.currentTimeMillis())

  val cards = new Array[Int](52)
  for (i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1

  var total = 0
  try{
    for (i <- 0 until 52) {
      try {
        if (cards(i) == -1) throw new Exception
        total = total + cards(i)
      } catch {
        case _: Throwable =>
      }
      if(total>=21) throw new Exception
      }
  }
  catch {
    case _: Throwable =>
  }
  // iterate through cards incrementing total, use break to continue and break 

  println("total = " + total)
}

  
  