package main.scala

object Util{
	def square(x: Double) = x * x
	def doubleToColorFloat(f: Double): Float = math.min(1, math.max(0, f)).asInstanceOf[Float]
	def minRoot(a: Double, b: Double, c: Double): Option[Double] = {
		if(a == 0){
			Some[Double](-c / b)
		}else{
			val disc = square(b) - (4 * a * c)
			if(disc > 0){
				val discRoot = math.sqrt(disc)
				Some[Double](math.min((-b + discRoot) / (2 * a),
															(-b - discRoot) / (2 * a)))
			}else{
				None
			}
		}
	}

  val COLOR_BLACK = V3(0.0, 0.0, 0.0)
  val COLOR_WHITE = V3(1.0, 1.0, 1.0)
  val COLOR_RED = V3(1.0, 0.0, 0.0)
  val COLOR_GREEN = V3(0.0, 1.0, 0.0)
  val COLOR_BLUE = V3(0.0, 0.0, 1.0)
  val COLOR_GREY = V3(0.5, 0.5, 0.5)
}
