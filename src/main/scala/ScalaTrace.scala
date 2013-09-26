import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}
import javax.swing.{JFrame, JPanel}

abstract trait WorldObject{
	def normal(point: V3): V3
	def intersect(origin: V3, Ray: V3): Option[V3]
	def colorAt(loc: V3): V3
}

class Sphere(val center: V3, val radius: Double, val color: V3) extends WorldObject{
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
			Some[V3](
				V3(origin.x + (ray.x * n.get),
					 origin.y + (ray.y * n.get),
					 origin.z + (ray.z * n.get)))
		}else{
			None
		}
	}
	def colorAt(loc: V3) =  color
}

class Light(val loc: V3, val power: Double)
class Scene(val eye: V3, val lights: List[Light], val objects: List[WorldObject], val sky: V3, val ambient: V3){
  /**
    * allows you to copy the scene, setting named parameters differently
    */
  def copyWith(eye: V3 = eye, lights: List[Light] = lights, objects: List[WorldObject] = objects,
    sky: V3 = sky, ambient: V3 = ambient){
    new Scene(eye, lights, objects, sky, ambient)
  }
}

case class HitInfo(val hitLoc: V3, val obj: WorldObject, val ray: V3)

class ScalaTrace(scene: Scene){
	def firstHit(origin: V3, ray: V3): Option[HitInfo] = {
		def hitsFolder(acc: List[HitInfo], obj: WorldObject)={
			val intersect = obj.intersect(origin, ray)
			if(intersect != None){
				HitInfo(intersect.get, obj, ray) :: acc
			}else{
				acc
			}
		}
		
		val objects = scene.objects
		val hits = objects.foldLeft(List[HitInfo]())(hitsFolder)
		if(hits.length != 0){
			Some(hits.sortWith((_.hitLoc.dist(origin) < _.hitLoc.dist(origin)))(0))
		}else{
			None
		}
	}
	
	def lambert(obj: WorldObject, intersect: V3, ray: V3): Double = {
		val normal = obj.normal(intersect)
		(ray.norm dot normal) / Util.square(0.002 * ray.mag)
	}
	
	def sendRay(origin: V3, ray: V3): V3 = {
		val hit = firstHit(origin, ray)
		if(hit.isEmpty){
			scene.sky
		}else{
			def lightToColor(hit: HitInfo, light: Light): V3 = {
        val HitInfo(loc, obj, ray) = hit
				(obj.colorAt(loc) * light.power) * lambert(obj, loc, loc - light.loc)
			}
			scene.lights.map(lightToColor(hit.get,_))
			.foldRight(V3(0,0,0))(_+_) + scene.ambient
		}
	}
	
	def colorAt(vals: (Int, Int)): V3 = colorAt(vals._1, vals._2)
	def colorAt(x: Int, y: Int): V3 = {
		val ray = (V3(x, y, 0) - scene.eye).norm
		sendRay(scene.eye, ray)
	}
	
	def rayTrace(w: Int, h: Int): BufferedImage = {
    val start = System.currentTimeMillis
		val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
		val coords = for(x <- 0 until w; y <- 0 until h) yield (x, y)
    val perChunk = coords.length / Runtime.getRuntime.availableProcessors
    //group the coords into chunks of <perChunk>, turn the iterator into a list, into a parallel collection
		val colors = coords.grouped(perChunk).toList.par
      //and map mapping the colorAt fn on them
      .map(_.map((loc: (Int, Int)) => (loc, colorAt(loc))))
      //then collect them using the foldLeft(List[...]())(_++_)
      .foldLeft(List[((Int, Int), V3)]())(_++_)
		for(entry <- colors){
			val ((x, y), color) = entry
			img.setRGB(x, y, new Color(Util.doubleToColorFloat(color.x), 
																 Util.doubleToColorFloat(color.y), 
																 Util.doubleToColorFloat(color.z)).getRGB)
		}
    println("Took: " + (System.currentTimeMillis - start) + "ms")
		img
	}
}

object ScalaTrace {
	def main(args: Array[String]):Unit = {
		val scene1 = new Scene(V3(150, 150, 200),
													 List[Light](new Light(V3(-600, 150, -400), 0.8)),
													 List[WorldObject](
														 new Sphere(V3(100, 150, -400), 50, V3(0.8,0.8,0.1)),
														 new Sphere(V3(125, 150, -600), 100, V3(0.8,0.8,0.1)),
														 new Sphere(V3(175, 150, -800), 200, V3(0.8,0.8,0.1)),
														 new Sphere(V3(250, 150, -1200), 400, V3(0.8,0.8,0.1))
													 ),
													 V3(0.0, 0.0, 0.5), V3(0.1, 0.1, 0.1))
		val img = new ScalaTrace(scene1).rayTrace(300, 300)
		viewImage(img)
	}
	
	def viewImage(img: BufferedImage) = {
		val frame = new JFrame("Ray Tracer")
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
}

//ScalaTrace.main(Array[String]())
