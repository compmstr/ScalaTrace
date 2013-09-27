abstract class WorldObject(val shaders: List[Shader]){
  def normal(point: V3): V3
  def intersect(origin: V3, Ray: V3): Option[V3]
  def colorAt(scene: Scene, hit: HitInfo): V3 = {
    shaders.map(_.getColor(scene, hit)).map(Util.colorClamp).foldLeft(Util.COLOR_BLACK)(_ + _)
  }
  def shaderLocAt(loc: V3): (Double, Double)
}

class Sphere(val center: V3, val radius: Double, override val shaders: List[Shader]) extends WorldObject(shaders){
  def normal(point: V3) = {
    (center - point).norm
  }
  def intersect(origin: V3, ray: V3): Option[V3] = {
    val a = ray dot ray
    val b = 2 * ((origin - center) dot ray)
    val originToCenter = origin - center
    val c = (originToCenter dot originToCenter) - Util.square(radius)
    val n = Util.minRoot(a, b, c)
    if(!n.isEmpty){
      if(n.get > 0){
        Some[V3](
          V3(origin.x + (ray.x * n.get),
            origin.y + (ray.y * n.get),
            origin.z + (ray.z * n.get)))
      }else{
        None
      }
    }else{
      None
    }
  }
  def shaderLocAt(loc: V3) = {
    val vecUp = this.normal(new V3(center.x, center.y + 1, center.z))
    val vecFront = this.normal(new V3(center.x, center.y, center.z + 1))
    val locNorm = this.normal(loc)

    val frontAngle = math.acos(vecFront dot locNorm)
    val upAngle = math.acos(vecUp dot locNorm)
    (frontAngle % (math.Pi * 2), upAngle % (math.Pi * 2))
  }
}
