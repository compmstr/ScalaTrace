import scala.util.Random

/**
 * Camera class for Raytracer
 *
 * @param loc - location of center of near plane of frustum
 * @param orientation - direction camera faces
 * @param imageSize - image size in pixels (w/h)
 * @param width - width of near plane of frustum
 * @param fov - field of view (horizontal)
 * @param samples - number of samples to take per ray
 * @param jitter - amount of jitter to use
 * @param focalDist - how far away from the near plane of frustum to focus
 */
class Camera(val loc: V3, val orientation: V3, val imageSize: (Int, Int), val width: Double, val fov: Double,
             val samples: Int, val jitter: Double, val focalDist: Option[Double] = None, val rand: Random = new Random(12345)) {
  val origin = calcOrigin()
  protected val aspectRatio = calcAspectRatio()
  protected val invAspectRatio = 1 / aspectRatio
  val (xPixelStep, yPixelStep) = calcPixelSteps()
  println("origin: " + origin)
  println("orientation: " + orientation)
  println("aspect ratio: " + aspectRatio)
  println("pixel steps: " + (xPixelStep, yPixelStep))
  def copyWith(loc: V3 = loc, orientation: V3 = orientation, imageSize: (Int, Int) = imageSize, width: Double = width, fov: Double = fov,
               samples: Int = samples, jitter: Double = jitter, focalDist: Option[Double] = focalDist): Camera = {
    new Camera(loc, orientation, imageSize, width, fov, samples, jitter, focalDist)
  }

  /**
   * Returns a new camera, with the orientation pointed at the target point
   * @param target
   * @return
   */
  def lookAt(target: V3): Camera = {
    val newOrientation: V3 = (target - loc).norm
    copyWith(orientation = newOrientation)
  }

  protected def calcAspectRatio(): Double = {
    imageSize._1 / imageSize._2
  }
  protected def calcOrigin(): V3 = {
    val origDist = (imageSize._1 / 2) / math.tan(fov / 2)
    loc + (orientation * (-origDist))
  }
  protected def calcPixelSteps(): (Double, Double) = {
    val xStep = width / (imageSize._1 - 1)
    val yStep = xStep * invAspectRatio
    (xStep, yStep)
  }

  def applyJitter(vec: V3): V3 = {
    val halfJitter = jitter * 0.5;
    V3(vec.x + (jitter * rand.nextDouble()) - halfJitter,
      vec.y + (jitter * rand.nextDouble()) - halfJitter,
      vec.z + (jitter * rand.nextDouble()) - halfJitter)
  }

  def genRaySamples(base: (V3, V3)): List[(V3, V3)] = {
    val (baseOrigin, baseRay) = base
    focalDist match {
      case None =>
        (for(i <- 1 to samples) yield (applyJitter(baseOrigin), baseRay)).toList
      case Some(dist: Double) =>
        //TODO: update this to do actual focal blur
        (for(i <- 1 to samples) yield (applyJitter(baseOrigin), baseRay)).toList
    }
  }

  /**
   * Returns a map of screen coordinates to the ray for that coordinate
   * @return
   */
  def getRays(): Map[(Int, Int), List[(V3, V3)]] = {
    val halfWidth = width / 2;
    val height = width * invAspectRatio
    val halfHeight = halfWidth * invAspectRatio;
    println("halfWidth: " + halfWidth + " -- halfHeight: " + halfHeight)

    val globalUp = V3(0.0, 1.0, 0.0)
    val right = orientation.cross(globalUp).norm
    val up = orientation.cross(right).norm
    println("Right vector: " + right)
    println("Up vector: " + up)

    val left = loc - (right * (halfWidth * imageSize._1))
    val top = loc + (up * (halfHeight * imageSize._2))
    println("Top: " + top)
    println("Left: " + left)
    val planeStart = loc - (right * halfWidth) - (up * halfHeight)
    println("planeStart: " + planeStart)

    //TODO: all until here appear to be correct, however the actual coords generated are off
    //TODO: Probably need to use matrix rotation to get this to work properly
    (for(x <- 0 until imageSize._1; y <- 0 until imageSize._2) yield{
    //(for(deltaX <- (0.0 until width by xPixelStep); deltaY <- (0.0 until height by yPixelStep)) yield {
      val deltaX = x * xPixelStep
      val deltaY = y * yPixelStep
      //println("(x, y) -> " + (x, y) + " -- Deltas: " + (deltaX, deltaY))
      //println("Right vec: " + (right * deltaX) + " -- up vec: " + (up * deltaY))
      //println("Final pos: " + (planeStart + (right * deltaX) + (up * deltaY)))
      //println("===============================================")
      ((x, y), genRaySamples((origin, ((planeStart + (right * deltaX) + (up * deltaY)) - origin).norm)))
      //((x, y), List[(V3, V3)]((origin, ((planeStart + (right * deltaX) + (up * deltaY)) - origin).norm)))
    }).foldLeft(Map[(Int, Int), List[(V3, V3)]]())(_ + _)

    //return Map[(Double, Double), V3]()
  }
}
