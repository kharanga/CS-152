def inc(n: Int) = n + 1
def dec(n: Int) = n - 1
def isZero(n: Int) = n == 0

def add(n: Int, m: Int): Int = {
  if(isZero(m)){
    if(isZero(n)){
      0
    }
    else{
      inc(add(dec(n), m))
    }
  }
  else{
    inc(add(n, dec(m)))
  }
}

add(5, 10)
add(6, 3)
add(4, 2)

def mul(n: Int, m: Int): Int = {
  if(isZero(m)){
    0
  }
  else{
    add(n,mul(n,dec(m)))
  }
}

mul(5, 10)
mul(6, 3)
mul(4, 2)

def exp(m: Int): Int = {
  if(isZero(m)){
    1
  }
  else{
    mul(2, exp(dec(m)))
  }
}
exp(5)
exp(6)
exp(4)

def hyperExp(n: Int): Int = {
  if(isZero(n)){
    0
  }
  else{
    exp(hyperExp(dec(n)))
  }
}
hyperExp(0)
hyperExp(1)
hyperExp(2)
hyperExp(3)
hyperExp(4)

def add2(n: Int, m: Int): Int ={
  def helper(x: Int, y: Int, result: Int): Int ={
    if(isZero(y)){
      if(isZero(x)){
        result
      }
      else{
        helper(dec(x), y, inc(result))
      }
    }
    else{
      helper(x, dec(y), inc(result))
    }
  }
  helper(n, m, 0)
}
add2(5, 10)
add2(6, 3)
add2(4, 2)

def mul2(n: Int, m: Int): Int = {
  def helper(x: Int, result: Int): Int ={
    if(isZero(x)){
      result
    }
    else{
      helper(dec(x), add2(result, n))
    }
  }
  helper(m, 0)
}
mul2(5, 10)
mul2(6, 3)
mul2(4, 2)

def exp2(m: Int): Int = {
  def helper(x: Int, result: Int): Int ={
    if(isZero(x)) {
      result
    }
    else{
      helper(dec(x), mul2(result, 2))
    }
  }
  helper(m, 1)
}
exp2(5)
exp2(6)
exp2(4)

def hyperExp2(n: Int): Int = {
  def helper(x: Int, result: Int): Int ={
    if(isZero(x)) {
      result
    }
    else{
      helper(dec(x), exp2(result))
    }
  }
  helper(n, 0)
}
hyperExp2(0)
hyperExp2(1)
hyperExp2(2)
hyperExp2(3)
hyperExp2(4)

def fib(n: Int): Int = {
  if(n<=1){
    n
  }
  else{
    fib(n-1) + fib(n-2)
  }
}
fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)

def fib2(n: Int): Int = {
  if(n<=1){
    n
  }
  else{
    def helper(x: Int, prevResult: Int, result: Int): Int ={
      if(x<n){
        helper(inc(x), result, prevResult + result)
      }
      else{
        result
      }
    }
    helper(2, 1, 1)
  }
}
fib2(0)
fib2(1)
fib2(2)
fib2(3)
fib2(4)
fib2(5)
fib2(6)

def choose(n: Int, m: Int): Int ={
  if(m == 0 || m == n) {
    1
  }
  else {
    choose(n - 1, m - 1) + choose(n - 1, m)
  }
}
choose(5, 4)
choose(6, 2)
choose(10, 7)

