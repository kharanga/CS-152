import scala.util._
import util.control.Breaks._

object BlackJack3 extends App {

  val gen = new Random(System.currentTimeMillis())

  val cards = new Array[Int](52)
  for (i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1

  var total = 0
  for (i <- 0 until 52 if total < 21 ) {
    if(cards(i)!= -1){
      total = total + cards(i)
    }
  }
  // iterate through cards incrementing total, use break to continue and break 

  println("total = " + total)
}

  
  