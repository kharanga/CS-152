object poly {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    val x = (0 -p._2 + (scala.math.sqrt(p._2 * p._2 - 4 * p._1 * p._3)))/(2*p._1)
    val y = (0 -p._2 - (scala.math.sqrt(p._2 * p._2 - 4 * p._1 * p._3)))/(2*p._1)
    val result = (x,y)
    result match{
      case  (x , y) => Some  (x,y)
    }
  }

  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
  // = derivative of p (which should be degree 1
    (0, 2*p._1, p._2)
  def eval(a: Double, p: (Double, Double, Double)): Double =
  // = p(a)
  p._1 * a * a + p._2 * a + p._3
}