//Q1
def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
  if(halt(state,cycle))state else controlLoop(update(state,cycle),cycle + 1, halt, update)

//Q2
controlLoop(1, 0, (x: Int, y: Int)=>x > Math.pow(10, 5), (x: Int, y: Int)=> x*2)

//Q3
def solve(f: Double=>Double): Double = {
  val DELTA = 1e-8
  def goodEnuf(guess: Double, cycle: Int): Boolean = math.abs(f(guess))<=DELTA
  def improve(currGuess: Double, cycle: Int) = currGuess - f(currGuess) / df(currGuess)
  def df(x: Double) = (f(x+DELTA)-f(x))/DELTA
  controlLoop(1.0, 0, goodEnuf, improve)
}

//Q4
def squareRoot(x: Double) = x * x
solve(squareRoot)

//Q5
def cubeRoot(x: Double) = x * x * x
solve(cubeRoot)

//Q6
def nthRoot(x: Double, n: Int) = {
    math.pow(x, n)
}
solve((x: Double)=> nthRoot(x, 5))

//Q7
def compoundInterest(periods : Int) = {
  def end(currVal: Double, time: Int) = time == periods
  val rate = 1.0 / periods
  def totalMoney(currVal: Double, age: Int) = currVal + rate * currVal
  controlLoop(1.0, 0, end, totalMoney)
}
compoundInterest(12)
compoundInterest(365)
compoundInterest(365 * 24)
compoundInterest(365 * 12 * 60 * 60)