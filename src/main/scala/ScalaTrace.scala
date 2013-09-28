import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}
import javax.swing.{JFrame, JPanel}

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

class ScalaTrace(scene: Scene){

  def v3ToColor(color: V3): Color = {
    val clamped = Util.colorClamp(color)
    new Color(clamped.x.asInstanceOf[Float],
      clamped.y.asInstanceOf[Float],
      clamped.z.asInstanceOf[Float])
  }
	def rayTrace(): BufferedImage = {
    val (w, h) = scene.cam.imageSize
    val start = System.currentTimeMillis
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val coordsAndRays = scene.cam.getRays()
    val perChunk = coordsAndRays.size / Runtime.getRuntime.availableProcessors
    //group the coords into chunks of <perChunk>, turn the iterator into a list, into a parallel collection
    val colors = coordsAndRays.grouped(perChunk).toList.par
      //and map mapping the sendRay fn on them
      .map(_.map((locRay: ((Int, Int), V3)) => (locRay._1, ScalaTrace.sendRay(scene, scene.cam.origin, locRay._2))))
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
	def main(args: Array[String]):Unit = {
    val greenShader = LambertShader(Util.COLOR_GREEN)
    val redShader = LambertShader(Util.COLOR_RED)
    val blueShader = LambertShader(Util.COLOR_BLUE)
    val whiteShader = LambertShader(Util.COLOR_WHITE)
    val greyShader = LambertShader(Util.COLOR_GREY)
    val cam1 = new Camera(loc = V3(150, 150, 500), orientation = V3(0, 0, -1), imageSize = (300, 300), width = 150, fov = math.Pi / 4, samples = 1, jitter = 0, focalDist = 0)
    val cam2 = cam1.lookAt(V3(-200, 150, -400)).copyWith(fov = math.Pi / 8)
		val scene1 = new Scene(cam = cam1,
													 lights = List[Light](new Light(V3(-600, 150, -400), 0.8)),
													 objects = List[WorldObject](
                             new Sphere(V3(0, 150, -500), 100, List(redShader)),
                             new Sphere(V3(100, 150, -400), 100, List(greenShader, new ReflectiveShader(0.5, 4))),
                             //new Sphere(V3(100, 150, -400), 100, List(greenShader)),
                             new Sphere(V3(-200, 150, -400), 100, List(greyShader, new ReflectiveShader(0.85, 4))),
                             new Sphere(V3(150, 150, -300), 100, List(whiteShader))
													 ),
													 sky = V3(0.0, 0.0, 0.5), ambient = V3(0.1, 0.1, 0.1))
		val img = new ScalaTrace(scene1).rayTrace()
		viewImage(img, title = "Cam 1")

    viewImage(new ScalaTrace(scene1.copyWith(cam = cam2)).rayTrace(), title = "Cam 2")
	}
	
	def viewImage(img: BufferedImage, title: String = "Ray Tracer") = {
		val frame = new JFrame(title)
		val panel = new JPanel{
			override def paintComponent(g: Graphics) = {
				g.drawImage(img, 0, 0, this)
			}
		}
		frame.add(panel)
		frame.setSize(img.getWidth, img.getHeight)
		frame.setResizable(false)
		frame.setVisible(true)
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
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
