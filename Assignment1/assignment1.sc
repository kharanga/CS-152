import scala.util.Random

// problem 1
def kingdom(n: Int) =
  if(n > 10){
    if(n % 2 == 0){
      if(n % 100 == 0){
        2
      }
      else{
        1
      }
    }
    else{
      4
    }
  }
  else{
    3
  }
kingdom(10)
kingdom(1500)
kingdom(2)
kingdom(33)
kingdom(22)

//problem 2
def order(n: Int): Int = {
  {
    var order = 0
    if (n >= 0) {
      if (n % 3 == 0) {
        order = 1
      }
      else {
        order = 2
      }
      if (n == 50) {
        order = order * 3
      }
      else {
        order = order * 4
      }
      if (n % 7 == 0) {
        order = order + 5
      }
      else {
        order = order + 6
      }
      order
    }
    else {
      0
    }
  }
}
order(6)
order(35)
order(21)
order(28)
order(50)

//problem 3
//The problem with this function is that if a number is negative it return nothing
//while the return value suppose to be 2
def species(n: Int) = if (0 < n) {if (n % 2 == 0) 1 else 2} else 2
species (1)
species (2)
species (-1)
species (-2)

//problem 4
def tax (n: Double) ={
  n match{
    case a if 0 until 20000 contains a => n * 0
    case b if 20000 until 30000 contains b=> n * .05
    case c if 30000 until 40000 contains c=> n * .11
    case d if 40000 until 50000 contains d=> n * .23
    case e if 50000 until 100000 contains e=> n * .32
    case f if f >100000 => n * .50
    case _ => throw new Exception("Invalid Income exception")
  }
}
tax(12300)
tax(29000)
tax(125000)
tax(1000000)
try {
  tax(-1000000)
} catch {
  case e: Exception => println(e)
}

// problem 5
def drawRectangle(x : Int, y: Int){
  for(i <- 0 until x){
    for(n <- 0 until y) {
      print("*")
    }
    print("\n")
  }
}
drawRectangle(3,4)
drawRectangle(2,7)


//problem 6
def printSums(x : Int, y: Int){
  for(i <- 0 until x){
    for(n <- 0 until y) {
      print (i + " + " + n + " = " + (i + n))
      print("\n")
    }
  }
}
printSums(3, 4)

//problem 8
def realm(n: Int)= 0

def realm3(n: Int)= {
  if (n % 6 == 0 && n % 7 == 0 && n > 0) {
    3
  }
  else {
    try{
      realm(n)
    }
    catch{
      case e: Exception => println(e)
    }
  }
}

def realm2(n: Int)= {
  if (n % 3 != 0 && n > 0){
    2
  }
  else {
    try{
      realm3(n)
    }
    catch{
      case e: Exception => println(e)
    }
  }
}

def realm1(n: Int)=  {
  if (n % 2 != 0 && n > 0){
    1
  }
  else {
     try{
       realm2(n)
     }
    catch{
      case e: Exception => println(e)
    }
  }
}
realm1(3)
realm1(4)
realm1(42)
realm1(-1)
// problem 9
def sqrtLog(x: Double ): Option[Double] ={
  val result = math.sqrt(math.log(x))
  x match{
    case a if a < 1 => None
    case b if b >= 1 => Some(result)
  }
}
sqrtLog(0)
sqrtLog(1)
sqrtLog(1.1)
sqrtLog(-1)

// Dice roll problem
def rollDice():(Int , Int) = {
  (Random.between(1,7), Random.between(1,7))
}
rollDice()
rollDice()
rollDice()
rollDice()
/*
val res17: (Int, Int) = (6,5)
val res18: (Int, Int) = (3,4)
val res19: (Int, Int) = (1,5)
val res20: (Int, Int) = (6,2)
 */

//String processing
//problem 1
def isPal(s : String) = s == s.reverse
isPal("rotator")
isPal("cat")
isPal("Civic")
isPal("Toyota")
/*
val res0: Boolean = true
val res1: Boolean = false
val res2: Boolean = false
val res3: Boolean = false
 */

def isPal2(s : String) = {
  var result = s.replaceAll("""[\p{Punct}||[,]||[" "]]""", "")
  result = result.toLowerCase()
  result == result.reverse
}
isPal2("A man, a plan, a canal, Panama!")

//problem 3
def mkWord(size: Int = 5) = {
  var result = ""
  for (i <- 0 until size) {
    result = result + Random.between(97, 123).toChar
  }
  result
}
mkWord()
mkWord(10)

def mkSentence(size: Int = 10) = {
  var result = "" + Random.between(65,91).toChar
  for(i <- 0 until size){
    result = result + mkWord(Random.between(1, 15)) + " "
  }
  result + "."
}
mkSentence()
mkSentence(5)
mkSentence(12)