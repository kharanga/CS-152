import scala.util.control.Breaks.breakable

object arithmetic {

  def sqrt(n: Int): Option[Int] = {
    var m = 0
    while(m * m <= n ){
      m = m + 1
    }
    n match{
      case a if a < 0 => None
      case b if b >= 0 => Some(m-1)
    }
  }

  def log(n: Int): Option[Int] = {
    var m = 0
    while(scala.math.pow(2,m) <= n){
      m = m + 1
    }
    n match{
      case a if a <=0 => None
      case b if b > 0 => Some(m-1)
    }
  }

  def isPrime(n: Int): Option[Boolean] = {
    var divisible = true
    for (i <-2 until n){
      if(n % i == 0){
        divisible = false
      }
    }
    n match{
      case a if a < 0 => None
      case b if b >= 0 => Some(divisible)
    }
  }

  def gcd(n: Int, m: Int): Option[Int] = {
    var smallest = n
    var result = 1
    if(m < n){
      smallest = m
    }
    for(i <- 2 to smallest){
      if(n % i == 0 && m % i == 0){
        result = i
      }
    }
    smallest match{
      case a if a <0 => None
      case b if b >= 0 => Some(result)
    }
  }

  def lcm(n: Int, m: Int): Option[Int] = {
    var smallest = n
    var result = m
    if(m < n){
      smallest = m
      result = n
    }
    while(result % m != 0 || result % n != 0){
      result = result + 1
    }
    smallest match{
      case a if a <0 => None
      case b if b >= 0 => Some(result)
    }
  }

  def phi(n: Int): Option[Int] = {
    var result = 0
    for(i<- 1 to n){
      if(gcd(i , n)==Some(1)){
        result = result + 1
      }
    }
    n match{
      case a if a < 0 => None
      case b if b >= 0 => Some(result)
    }
  }

}