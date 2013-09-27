abstract class Shader(){
  def getColor(scene: Scene, hit: HitInfo): V3
  protected def isInShadowForLight(hit: HitInfo, light: Light): Boolean = {
    val toLight = hit.intersect - light.loc
    val firstHit = ScalaTrace.firstHit(hit.scene, hit.intersect + toLight * 0.001, toLight)
    return !(firstHit.isEmpty)
  }

  /**
   * Filters a list of lights for a particular hit to weed out the ones we're in the shadow of
   *   If no lights are visible, returns a 0 power light, in order allow ambient to go through
   * @param hit
   * @param lights
   * @return
   */
  protected def shadowFilter(hit: HitInfo, lights: List[Light]): List[Light] = {
    lights.filter(!isInShadowForLight(hit, _))
  }
}

case class LambertShader(color: V3) extends Shader{
	private def lambert(scene: Scene, hit: HitInfo): Double = {
    val HitInfo(_, intersect, obj, _) = hit
		val normal = obj.normal(intersect)
    (for(light <- shadowFilter(hit, scene.lights))yield {
      val toLight = intersect - light.loc
      ((toLight.norm dot normal) / Util.square(0.002 * toLight.mag)) * light.power
    }).foldRight(0.0)(_+_)
	}
  override def getColor(scene: Scene, hit: HitInfo): V3 = {
    color * lambert(scene, hit)
  }
}

case class ReflectiveShader(amount: Double, maxBounces: Int = 1) extends Shader(){
  private def reflectRay(incoming: V3, normal: V3) = {
    ((normal * ((incoming dot normal) * 2)) - incoming).norm
  }
  override def getColor(scene: Scene, hit: HitInfo): V3 = {
    if(scene.recursion >= maxBounces){
      scene.sky
    }else{
      val newRay = reflectRay(hit.ray.norm, hit.obj.normal(hit.intersect)) * -1
      /*println("========================================")
      println("Reflective Shader -- recursion level: " + scene.recursion)
      println("Hit: " + hit.obj + " at: " + hit.intersect)
      println("Incoming ray: " + hit.ray)
      println("Normal: " + hit.obj.normal(hit.intersect))
      println("Reflective ray: " + newRay)
      println("New origin: " + (hit.intersect + (newRay * 0.1)))
      */
      val newColor = ScalaTrace.sendRay(scene.copyWith(recursion = scene.recursion + 1), hit.intersect + (newRay * 0.001), newRay)
      (newColor * (1.0 / math.pow(2, scene.recursion))) * amount
    }
  }
}