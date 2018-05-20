
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    println(s"Radius $radius")
//    println(s"x $x")
//    println(s"y $y")


    val xs = Array(x - radius, x + radius) map (v => clamp(v, 0, src.width - 1))
    val ys = Array(y - radius, y + radius) map (v => clamp(v, 0, src.height - 1))
//    println(xs.mkString(" "))
//    println(ys.mkString(" "))

    var redSum = 0
    var greenSum = 0
    var blueSum = 0
    var alphaSum = 0

    var pixelCount = 0

    var col = xs(0)
    while (col <= xs(1)) {
      var row = ys(0)
      while (row <= ys(1)) {
        val pixel = src(col, row)
        redSum += red(pixel)
        greenSum += green(pixel)
        blueSum += blue(pixel)
        alphaSum += alpha(pixel)
//        println(alphaSum)

        row += 1
        pixelCount += 1
      }
      col += 1
    }


    rgba(redSum / pixelCount,
      greenSum / pixelCount, blueSum / pixelCount, alphaSum / pixelCount)
  }
}
