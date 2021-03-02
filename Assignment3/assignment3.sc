//Q1
def compose[A,B,C](f:B=>C, g: A=>B): A=>C = (x: A) => f(g(x))

//Q2
def id[T](x: T) = x

def selfIter[T](f: T=>T, n: Int): T => T  = {
  if(n == 0){
    id(id _)
  }
  else{
    compose(f, selfIter(f, n-1))
  }
}
def inc(x: Double) = x + 1
val func1 = selfIter((x: Double)=>(inc(x)), 4)
func1(2)
def double(x: Double) = 2 * x
val func2 = selfIter((x: Double)=>(double(x)), 4)
func2(2)

//Q3
def countPass[T,S](inputs: Array[T], f: T=> Boolean): Int = {
  var result = 0
  for(x <- inputs) if (f(x) == true ) result += 1
  result
}

countPass(Array("true", false, true, false, 1, 2,"hello"), ((s: Any) => s.isInstanceOf[Boolean]))

//Q4
def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
  def helper(n: Int): Int = if(n == 0 )baseVal else combiner(n, helper(n-1))
  helper _
}
def fact(n: Int) ={
  recur(1, (x: Int, y: Int)=> x*y)(n)
}
fact(5)

//Q5
def deOptionize[T](digits: String, f: String=>Option[T]) = {
  f(digits: String)match {
    case Some(n) => n
    case None => throw new Exception("Invalid input")
  }
}

def parseDigits(digits: String): Option[Int] =
  if (digits.matches("[0-9]*")) Some(digits.toInt) else None

deOptionize("1234", parseDigits)
try{
  deOptionize("12x34", parseDigits)
}
catch{
  case e: Exception => e.getMessage
}

//Q6
def makeIter(f: (Double)=> Double): (Double, Int)=> Double ={
  def iterf(init: Double, n: Int)={
    var result = init
    for(i<- 0 until n) result = f(result)
    result
  }
  iterf _
}

def iterSquare(init: Double, n: Int) = {
  makeIter((x: Double)=> x * x)(init, n)
}
iterSquare(4, 3)

//Q7
def cube(n: Int) = n * n * n

def unitTest[T,S](f:T=>S, pairs: Array[(T, S)]): Int ={
  var result = 0
  for(i<- 0 until pairs.length){
    if(f(pairs(i)._1) != pairs(i)._2){
      result = result + 1
    }
  }
  result
}
unitTest(cube, Array((1, 1), (2, 8), (3, 9), (4, 64), (5, 124)))
