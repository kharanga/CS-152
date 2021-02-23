object VectorTest extends App {
  val v1 = (2.0, 2.0, 2.0)
  val v2 = (1.0, 0.0, 0.0)
  val v3 = (0.0, 1.0, 0.0)

  println("sum(v3, v2) = " + vector.sum(v3, v2))
  println("mul(3, v1) = " + vector.mul(3, v1))

  println("dot(v1, v2) = " + vector.dot(v1, v2))
  println("dot(v2, v3) = " + vector.dot(v2, v3))
  println("dot(v1, v1) = " + vector.dot(v1, v1))

  println("length(v1) = " + vector.length(v1))
  println("length(v2) = " + vector.length(v2))

  println("theta(v1, v2) = " + vector.theta(v1, v2))
  println("theta(v3, v2) = " + vector.theta(v3, v2))
  println("pi/2 = " + Math.PI/2)
}