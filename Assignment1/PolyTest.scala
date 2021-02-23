object PolyTest extends App {
  val p = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)

  println("eval(6, p) = " + poly.eval(6, p))
  println("eval(2, p) = " + poly.eval(2, p))
  println("eval(-5, p) = " + poly.eval(-5, p))

  println("roots(p) = " + poly.roots(p))

  println("deriv(p) = " + poly.deriv(p))
  println("deriv2(p) = " + poly.deriv(poly.deriv(p)))

}
