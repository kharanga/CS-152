//Khang Nguyen Cs152
//Q1
def cube1(number: List[Int]) ={
  var sum = 0
  for(i <- 0 until number.size){
    if(number(i)%2 != 0){
      sum = sum + number(i) * number(i) * number(i)
    }
  }
  sum
}

def cube2(number: List[Int]): Int ={
  if(number.isEmpty == true){
    0
  }
  else{
    if(number(0)%2 != 0) {
      number(0) * number(0) * number(0) + cube2(number.drop(1))
    }
    else{
      cube2(number.drop(1))
    }
  }
}

def cube3(number: List[Int]): Int ={
  def helper(number: List[Int], sum: Int): Int ={
    if(number.isEmpty == true){
      sum
    }
    else{
      if(number(0)%2 != 0) {
        helper(number.drop(1), number(0) * number(0) * number(0) + sum)
      }
      else{
        helper(number.drop(1), sum)
      }
    }
  }
  helper(number, 0)
}

def cube4(number: List[Int]): Int ={
  number.map(x => x*x*x).filter(x => x%2 != 0).reduce((x, y) => x + y)
}

val cube = List(1, 1, 2, 3, 5)
cube1(cube)
cube2(cube)
cube3(cube)
cube4(cube)

//Q2
def sumOfSums1(number: List[List[Int]]): Int = {
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

def sumOfSums2(number: List[List[Int]]): Int = {
  def helper(number: List[List[Int]], sum: Int): Int = {
    if (number.isEmpty) {
      sum
    }
    else {
      if (number(0).isEmpty) {
        helper(number.drop(1), sum)
      }
      else {
        val newFirstList = List(number(0).drop(1))
        val newNumber = newFirstList ++ number.drop(1)
        helper(newNumber, sum + number(0)(0))
      }
    }
  }
  helper(number, 0)
}


def sumOfSums3(number: List[List[Int]]): Int = {
  if(number.isEmpty){
    0
  }
  else{
    if(number(0).isEmpty){
      sumOfSums2(number.drop(1))
    }
    else{
      val newFirstList = List(number(0).drop(1))
      val newNumber = newFirstList ++ number.drop(1)
      number(0)(0) + sumOfSums2(newNumber)
    }
  }
}

def sumOfSums4(number: List[List[Int]]): Int = {
  number.flatten.reduce((x,y)=> x + y)
}

sumOfSums1(List(List(1, 2, 3), List(4, 5, 6)))
sumOfSums2(List(List(1, 2, 3), List(4, 5, 6)))
sumOfSums3(List(List(1, 2, 3), List(4, 5, 6)))
sumOfSums4(List(List(1, 2, 3), List(4, 5, 6)))

//Q6
def isEven(number : Int) = if(number % 2 == 0) true else false
def predicateCount1[T](list : List[T], predicate: T=>Boolean): Int ={
  var count = 0
  for(i <- 0 until list.size) {
    if (predicate(list(i))) count = count + 1
  }
  count
}

def predicateCount2[T](list : List[T], predicate: T=>Boolean): Int ={
  if(list.isEmpty) 0
  else{
    if(predicate(list(0))) 1 + predicateCount1(list.drop(1), predicate) else predicateCount1(list.drop(1), predicate)
  }
}

def predicateCount3[T](list : List[T], predicate: T=>Boolean): Int ={
  def helper[T](list : List[T], predicate: T=>Boolean, count : Int): Int ={
    if(list.isEmpty) count
    else{
      if(predicate(list(0))) helper(list.drop(1), predicate, count+1) else helper(list.drop(1), predicate, count)
    }
  }
  helper(list, predicate , 0)
}

def predicateCount4[T](list : List[T], predicate: T=>Boolean): Int ={
  list.filter(predicate).length
}
predicateCount1(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateCount2(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateCount3(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateCount4(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)

//Q7
def predicateAll1[T](list : List[T], predicate: T=>Boolean): Boolean = {
  var result = true
  for (i <- 0 until list.size) {
    if (predicate(list(i)) == false) result = false
  }
  result
}

def predicateAll2[T](list : List[T], predicate: T=>Boolean): Boolean = {
  if(list.isEmpty){
    true
  }
  else {
    if (predicate(list(0))) true && predicateAll2(list.drop(1), predicate) else false && predicateAll2(list.drop(1), predicate)
  }
}

def predicateAll3[T](list : List[T], predicate: T=>Boolean): Boolean = {
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

def predicateAll4[T](list : List[T], predicate: T=>Boolean): Boolean = {
  list.filter(predicate).size == list.size
}
predicateAll1(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAll1(List(2, 8, 12, 14, 16), isEven)
predicateAll2(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAll2(List(2, 8, 12, 14, 16), isEven)
predicateAll3(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAll3(List(2, 8, 12, 14, 16), isEven)
predicateAll4(List(1, 2, 3, 5, 8, 12, 14, 16), isEven)
predicateAll4(List(2, 8, 12, 14, 16), isEven)

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
def spellCheck1(doc: List[String], dictionary: List[String]): List[String] = {
  var mispelled = List[String]()
  for(i <- 0 until doc.size){
    if(!dictionary.contains(doc(i))) mispelled = doc(i) :: mispelled
  }
  mispelled
}
spellCheck1(List("Khang", "Nguyen", "Hello", "World"), List("Khang", "World"))

def spellCheck2(doc: List[String], dictionary: List[String]): List[String] = {
  doc.filter(x => !dictionary.contains(x))
}

spellCheck2(List("Khang", "Nguyen", "Hello", "World"), List("Khang", "World"))

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