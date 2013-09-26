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
}
