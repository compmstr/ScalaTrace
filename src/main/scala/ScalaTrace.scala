package main.scala

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}
import javax.swing.{JFrame, JPanel}

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

case class HitInfo(intersect: V3, obj: WorldObject, ray: V3)

class ScalaTrace(scene: Scene, recursion: Int = 0){
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
			Some(hits.sortWith((_.intersect.dist(origin) < _.intersect.dist(origin)))(0))
		}else{
			None
		}
	}

	
	def sendRay(origin: V3, ray: V3): V3 = {
		val hit = firstHit(origin, ray)
		if(hit.isEmpty){
			scene.sky
		}else{
      hit.get.obj.colorAt(scene, hit.get) + scene.ambient
      /*
			def lightToColor(hit: HitInfo, light: Light): V3 = {
        val HitInfo(loc, obj, ray) = hit
				(obj.colorAt(loc) * light.power) * lambert(obj, loc, loc - light.loc)
			}
			scene.lights.map(lightToColor(hit.get,_))
			.foldRight(V3(0,0,0))(_+_) + scene.ambient
			*/
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
    val greenShader = LambertShader(Util.COLOR_GREEN)
    val redShader = LambertShader(Util.COLOR_RED)
    val blueShader = LambertShader(Util.COLOR_BLUE)
		val scene1 = new Scene(V3(150, 150, 200),
													 List[Light](new Light(V3(-600, 150, -400), 0.8)),
													 List[WorldObject](
                             new Sphere(V3(0, 150, -500), 100, List(redShader)),
                             new Sphere(V3(100, 150, -400), 100, List(greenShader)),
                             new Sphere(V3(200, 150, -300), 100, List(blueShader))
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
