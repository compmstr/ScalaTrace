case class V3(val x: Double, val y: Double, val z: Double){
	def parts() = List(x,y,z)
	def +(other: V3) = {
		V3.fromList((parts, other.parts).zipped map (_ + _))
	}
	def -(other: V3) = {
		V3.fromList((parts, other.parts).zipped map (_ - _))
	}
	def *(scale: Double) = {
		V3.fromList(parts.map(_*scale))
	}
	def dot(other:V3) = {
		(parts, other.parts).zipped.map (_ * _).foldRight(0.0)(_ + _)
	}
	def mag = math.sqrt(dot(this))
	def norm = this * (1.0 / this.mag)
	def dist(other: V3) = (this - other).mag
	def cross(other :V3) = {
		new V3((y * other.z) - (z * other.y),
					 (z * other.x) - (x * other.z),
					 (x * other.y) - (y * other.x))
	}
}
object V3{
	def fromList(lst: List[Double]): V3 = {
		new V3(lst(0), lst(1), lst(2))
	}
}
