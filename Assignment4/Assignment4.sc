//Khang Nguyen Cs152
//Q1
def cubeIterative(number: List[Int]) ={
  var sum = 0
  for(i <- 0 until number.size){
    if(number(i)%2 != 0){
      sum = sum + number(i) * number(i) * number(i)
    }
  }
  sum
}

def cubeClassical(number: List[Int]): Int ={
  if(number.isEmpty){
    0
  }
  else{
    if(number.head%2 != 0) {
      number.head * number.head * number.head + cubeClassical(number.drop(1))
    }
    else{
      cubeClassical(number.drop(1))
    }
  }
}

def cubeTail(number: List[Int]): Int ={
  def helper(number: List[Int], sum: Int): Int ={
    if(number.isEmpty){
      sum
    }
    else{
      if(number.head%2 != 0) {
        helper(number.drop(1), number.head * number.head * number.head + sum)
      }
      else{
        helper(number.drop(1), sum)
      }
    }
  }
  helper(number, 0)
}

def cubePipeline(number: List[Int]): Int ={
  number.map(x => x*x*x).filter(x => x%2 != 0).reduce((x, y) => x + y)
}

val cube = List(1, 1, 2, 3, 5)
cubeIterative(cube)
cubeClassical(cube)
cubeTail(cube)
cubePipeline(cube)

//Q2
def sumOfSumsIterative(number: List[List[Int]]): Int = {
  var combinedList = number(0)
  var sum = 0
  for(i <- 1 until number.size){
    combinedList = combinedList ++ number(i)
  }
  for(j <- 0 until combinedList.size){
    sum = sum + combinedList(j)
  }
  sum
}

def sumOfSumsClassical(number: List[List[Int]]): Int = {
  if(number.isEmpty){
    0
  }
  else{
    if(number.head.isEmpty){
      sumOfSumsClassical(number.drop(1))
    }
    else{
      val newFirstList = List(number.head.drop(1))
      val newNumber = newFirstList ++ number.drop(1)
      number.head.head + sumOfSumsClassical(newNumber)
    }
  }
}

def sumOfSumsTail(number: List[List[Int]]): Int = {
  def helper(number: List[List[Int]], sum: Int): Int = {
    if (number.isEmpty) {
      sum
    }
    else {
      if (number.head.isEmpty) {
        helper(number.drop(1), sum)
      }
      else {
        val newFirstList = List(number.head.drop(1))
        val newNumber = newFirstList ++ number.drop(1)
        helper(newNumber, sum + number.head.head)
      }
    }
  }
  helper(number, 0)
}

def sumOfSumsPipeline(number: List[List[Int]]): Int = {
  number.flatten.reduce((x,y)=> x + y)
}

sumOfSumsIterative(List(List(1, 2, 3), List(4, 5, 6)))
sumOfSumsClassical(List(List(1, 2, 3), List(4, 5, 6)))
sumOfSumsTail(List(List(1, 2, 3), List(4, 5, 6)))
sumOfSumsPipeline(List(List(1, 2, 3), List(4, 5, 6)))

//Q6
def isEven(number : Int) = if(number % 2 == 0) true else false
def predicateCountIterative[T](list : List[T], predicate: T=>Boolean): Int ={
  var count = 0
  for(i <- 0 until list.size) {
    if (predicate(list(i))) count = count + 1
  }
  count
}

def predicateCountClassical[T](list : List[T], predicate: T=>Boolean): Int ={
  if(list.isEmpty) 0
  else{
    if(predicate(list(0))) 1 + predicateCountClassical(list.drop(1), predicate) else predicateCountClassical(list.drop(1), predicate)
  }
}

def predicateCountTail[T](list : List[T], predicate: T=>Boolean): Int ={
  def helper[T](list : List[T], predicate: T=>Boolean, count : Int): Int ={
    if(list.isEmpty) count
    else{
      if(predicate(list(0))) helper(list.drop(1), predicate, count+1) else helper(list.drop(1), predicate, count)
    }
  }
  helper(list, predicate , 0)
}

def predicateCountPipeline[T](list : List[T], predicate: T=>Boolean): Int ={
  list.filter(predicate).length
}
predicateCountIterative(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateCountClassical(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateCountTail(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateCountPipeline(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)

//Q7
def predicateAllIterative[T](list : List[T], predicate: T=>Boolean): Boolean = {
  var result = true
  for (i <- 0 until list.size) {
    if (predicate(list(i)) == false) result = false
  }
  result
}

def predicateAllClassical[T](list : List[T], predicate: T=>Boolean): Boolean = {
  if(list.isEmpty){
    true
  }
  else {
    if (predicate(list(0))) true && predicateAllClassical(list.drop(1), predicate) else false && predicateAllClassical(list.drop(1), predicate)
  }
}

def predicateAllTail[T](list : List[T], predicate: T=>Boolean): Boolean = {
  def helper[T](list : List[T], predicate: T=>Boolean, result: Boolean): Boolean ={
    if(list.isEmpty){
      result
    }
    else {
      if (predicate(list(0))) helper(list.drop(1), predicate, result && true) else  helper(list.drop(1), predicate, false)
    }
  }
  helper(list, predicate, true)
}

def predicateAllPipeline[T](list : List[T], predicate: T=>Boolean): Boolean = {
  list.filter(predicate).size == list.size
}
predicateAllIterative(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAllIterative(List(2, 8, 12, 14, 16), isEven)
predicateAllClassical(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAllClassical(List(2, 8, 12, 14, 16), isEven)
predicateAllTail(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAllTail(List(2, 8, 12, 14, 16), isEven)
predicateAllPipeline(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAllPipeline(List(2, 8, 12, 14, 16), isEven)

//Q13
def longListOf1From(): LazyList[Int] = 1 #:: longListOf1From()
val longListOf1 = longListOf1From().take(7)
longListOf1.toList

def listOfNoneNegativeFrom(number: Int): LazyList[Int] = number #:: listOfNoneNegativeFrom(number + 1)
val listOfNoneNegative = listOfNoneNegativeFrom(1).take(7)
listOfNoneNegative.toList

def listOfNoneNegativeEvenFrom(number: Int): LazyList[Int] = number #:: listOfNoneNegativeEvenFrom(number + 2)
val listOfNoneNegativeEven = listOfNoneNegativeEvenFrom(2).take(7)
listOfNoneNegativeEven.toList

def listOfSquaresFrom(number: Int): LazyList[Int] = (number * number) #:: listOfSquaresFrom(number + 1)
val listOfSquares = listOfSquaresFrom(1).take(7)
listOfSquares.toList

//Q15
def spellCheckIteratvie(doc: List[String], dictionary: List[String]): List[String] = {
  var mispelled = List[String]()
  for(i <- 0 until doc.size){
    if(!dictionary.contains(doc(i))) {
      if(!mispelled.contains(doc(i))) mispelled = doc(i) :: mispelled
    }
  }
  mispelled
}
spellCheckIteratvie(List("Khang", "Nguyen", "Hello", "Hello", "World"), List("Khang", "World"))

def spellCheckPipeline(doc: List[String], dictionary: List[String]): List[String] = {
  val mispelled = doc.filter(x => !dictionary.contains(x))
  mispelled.toSet.toList
}

spellCheckPipeline(List("Khang", "Nguyen", "Hello", "Hello", "World"), List("Khang", "World"))

//Q16
def evalMono(mono: (Double, Double), x: Double): Double = {
  mono._1 * Math.pow(x, mono._2)
}

def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
  var sum = 0.0
  for(i <- 0 until poly.size){
    sum = sum + evalMono(poly(i), x)
  }
  sum
}

evalPoly(List((3.0, 2.0), (2.0, 1.0), (-5.0, 0.0)), 3)