package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var {
      if(delta() < 0) Set()
      else if(delta() == 0) Set((-1.0*b())/(2.0*a()))
      else Set((-1.0*b()+Math.sqrt(delta()))/(2.0*a()), (-1.0*b()-Math.sqrt(delta()))/(2.0*a()))
    }
  }
}
