package main.scala

trait Shader {
  def getColor(scene: Scene, hit: HitInfo): V3
}

case class LambertShader(color: V3) extends Shader{
	private def lambert(scene: Scene, hit: HitInfo): Double = {
    val HitInfo(intersect, obj, _) = hit
		val normal = obj.normal(intersect)
    (for(light <- scene.lights)yield {
      val toLight = intersect - light.loc
      ((toLight.norm dot normal) / Util.square(0.002 * toLight.mag)) * light.power
    }).foldRight(0.0)(_+_)
	}
  override def getColor(scene: Scene, hit: HitInfo): V3 = {
    color * lambert(scene, hit)
  }
}

case class ReflectiveShader(amount: Double, maxBounces: Int = 1) extends Shader{
  private def reflectRay(incoming: V3, normal: V3){
    incoming - (n * ((incoming dot normal) * 2))
  }
  override def getColor(scene: Scene, hit: HitInfo): V3 = {
    if(scene.recursion >= maxBounces){
      scene.ambient
    }else{
      val newRay = reflectRay(hit.ray, hit.obj.normal(hit.intersect))
      new ScalaTrace(scene, scene.recursion + 1).sendRay(hit.intersect, newRay)
    }
  }
}