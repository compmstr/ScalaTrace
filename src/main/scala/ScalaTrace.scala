import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}
import javax.swing.{JFrame, JPanel}
import scala.actors.Actor

class Light(val loc: V3, val power: Double)
class Scene(val cam: Camera, val lights: List[Light], val objects: List[WorldObject],
            val sky: V3, val ambient: V3, val recursion: Int = 0){
  /**
    * allows you to copy the scene, setting named parameters differently
    */
  def copyWith(cam: Camera = cam, lights: List[Light] = lights, objects: List[WorldObject] = objects,
               sky: V3 = sky, ambient: V3 = ambient, recursion: Int = recursion): Scene = {
    new Scene(cam, lights, objects, sky, ambient, recursion)
  }
}

case class HitInfo(scene: Scene, intersect: V3, obj: WorldObject, ray: V3)

class ScalaTrace(val scene: Scene, val progress: Option[ProgressNotifier] = None){

  if(!progress.isEmpty) progress.get.start

  def v3ToColor(color: V3): Color = {
    val clamped = Util.colorClamp(color)
    new Color(clamped.x.toFloat,
      clamped.y.toFloat,
      clamped.z.toFloat)
  }
	def rayTrace(): BufferedImage = {
    val (w, h) = scene.cam.imageSize
    val start = System.currentTimeMillis
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val coordsAndRays = scene.cam.getRays()
    val perChunk = coordsAndRays.size / Runtime.getRuntime.availableProcessors
    if(progress.isDefined) progress.get.reset
    //group the coords into chunks of <perChunk>, turn the iterator into a list, into a parallel collection
    val colors = coordsAndRays.grouped(perChunk).toList.par
      //and map mapping the sendRay fn on them
      .map(_.map((locRay: ((Int, Int), List[(V3, V3)])) => {
      val samples = locRay._2.map((curRay: (V3, V3)) => ScalaTrace.sendRay(scene, curRay._1, curRay._2))
      val numSamples = locRay._2.size
      //Update the progress bar if needed
      if(!progress.isEmpty) progress.get ! 1
      (locRay._1, samples.foldLeft(V3(0, 0, 0))(_ + _) * (1.0 / numSamples))
      //(locRay._1, ScalaTrace.sendRay(scene, scene.cam.origin, locRay._2))
    }))
      //then collect them using the foldLeft(List[...]())(_++_)
      .foldLeft(List[((Int, Int), V3)]())(_++_)

		/*val coords = for(x <- 0 until w; y <- 0 until h) yield (x, y)
    //group the coords into chunks of <perChunk>, turn the iterator into a list, into a parallel collection
		val colors = coords.grouped(perChunk).toList.par
      //and map mapping the colorAt fn on them
      .map(_.map((loc: (Int, Int)) => (loc, colorAt(loc))))
      //then collect them using the foldLeft(List[...]())(_++_)
      .foldLeft(List[((Int, Int), V3)]())(_++_)*/
		for(entry <- colors){
			val ((x, y), color) = entry
			img.setRGB(x, y, v3ToColor(color).getRGB)
		}
    println("Took: " + (System.currentTimeMillis - start) + "ms")
		img
	}
}

object ScalaTrace {
  val greenShader = LambertShader(Util.COLOR_GREEN)
  val redShader = LambertShader(Util.COLOR_RED)
  val blueShader = LambertShader(Util.COLOR_BLUE)
  val whiteShader = LambertShader(Util.COLOR_WHITE)
  val greyShader = LambertShader(Util.COLOR_GREY)

  def getTestScene: Scene = {
    val cam = new Camera(loc = V3(150, 150, 500), orientation = V3(0, 0, -1), imageSize = (300, 300),
      width = 150, fov = math.Pi / 4, samples = 1, jitter = 0)
    val scene = new Scene(cam = cam,
      lights = List[Light](
        new Light(V3(-600, 150, -400), 0.8),
        new Light(V3(600, 150, -150), 0.5),
        new Light(V3(600, 150, 0), 0.25)
      ),
      objects = List[WorldObject](
        new Sphere(V3(0, 150, -500), 100, List(redShader)),
        new Sphere(V3(100, 150, -400), 100, List(greenShader, new ReflectiveShader(0.5, 4))),
        //new Sphere(V3(100, 150, -400), 100, List(greenShader)),
        new Sphere(V3(-200, 150, -400), 100, List(greyShader, new ReflectiveShader(0.85, 4))),
        new Sphere(V3(150, 150, -300), 100, List(new CheckeredShader()))
      ),
      sky = V3(0.0, 0.0, 0.5), ambient = V3(0.1, 0.1, 0.1))

    scene
  }

	def main(args: Array[String]):Unit = {
    val scene1 = getTestScene
    val cam1 = scene1.cam

		//TracerGUI.viewImage(new ScalaTrace(scene1).rayTrace(), title = "Cam 1")

    //Do antialiasing
    val cam2 = cam1.lookAt(V3(-200, 150, -400)).copyWith(fov = math.Pi / 8, samples = 4, jitter = 1.0)
    //TracerGUI.viewImage(new ScalaTrace(scene1.copyWith(cam = cam2)).rayTrace(), title = "Cam 2")

    val notifier = new ProgressNotifier(300 * 300){
      var lastPercentEchoed = -1.0
      override protected def onUpdate() {
        super.onUpdate()
        if((lastPercentEchoed + 0.05) < percentDone()){
          println("%.2f%%" format(percentDone * 100))
          lastPercentEchoed = percentDone
        }
      }
      override def reset() {
        super.reset()
        lastPercentEchoed = -1.0
      }
      override def onDone(){
        println("Done!")
      }
    }
    //Do Focal Blur
    val cam3 = cam1.lookAt(V3(0, 150, -400)).copyWith(width = 200, fov = math.Pi / 8, samples = 12, jitter = 50.0, focalDist = Some(900))
    //TracerGUI.viewImage(new ScalaTrace(scene1.copyWith(cam = cam3), progress = Some(notifier)).rayTrace(), title = "Cam 3")
    //val gui = new TracerGUI(scene1.copyWith(cam = cam3))
    //val gui = new TracerGUI(scene1.copyWith(cam = cam2))
    val gui = new TracerGUI(scene1.copyWith(cam = scene1.cam.lookAt(V3(0,150,-500))))

    //Full 180 degrees (Pi Radians) makes a flat plane out of the camera, and so doesn't render anything
    //val cam3 = cam1.copyWith(loc = V3(150, 150, 0), fov = math.Pi - 0.1)
    //TracerGUI.viewImage(new ScalaTrace(scene1.copyWith(cam = cam3)).rayTrace(), title = "Cam 3")
	}
	
  def firstHit(scene: Scene, origin: V3, ray: V3): Option[HitInfo] = {
    def hitsFolder(acc: List[HitInfo], obj: WorldObject)={
      val intersect = obj.intersect(origin, ray)
      if(intersect != None){
        HitInfo(scene, intersect.get, obj, ray) :: acc
      }else{
        acc
      }
    }

    val objects = scene.objects
    val hits = objects.foldLeft(List[HitInfo]())(hitsFolder)
    if(hits.length != 0){
      Some(hits.sortWith(_.intersect.dist(origin) < _.intersect.dist(origin))(0))
    }else{
      None
    }
  }


  def sendRay(scene: Scene, origin: V3, ray: V3): V3 = {
    //println("sendRay origin: " + origin + " ray: " + ray)
    val hit = firstHit(scene, origin, ray)
    if(hit.isEmpty){
      scene.sky
    }else{
      hit.get.obj.colorAt(scene, hit.get) + scene.ambient
    }
  }

}

//ScalaTrace.main(Array[String]())
