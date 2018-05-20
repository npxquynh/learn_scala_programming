package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var row = from
    while (row < end) {
      var col = 0
      while (col < src.width) {
        val blurredPixel = boxBlurKernel(src, col, row, radius)
        dst.update(col, row, blurredPixel)

        col += 1
      }
      row += 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val range = 0 until src.height
    val step = (src.height * 1.0 / numTasks).ceil.toInt
    val splittingPoints = (range by step).toList
    val startEndTuples = (splittingPoints zip (splittingPoints.tail :+ src.height)).toArray

    // 2 tasks
    val startEndTuplesForTwoTasks = startEndTuples.grouped(2).toArray
    for {
      tuples <- startEndTuplesForTwoTasks
    } yield {
      if(tuples.size < 2) {
        blur(src, dst, tuples(0)._1, tuples(0)._2, radius)
      } else {
        val task1 = task {
          blur(src, dst, tuples(0)._1, tuples(0)._2, radius)
        }
        val task2 = task {
          blur(src, dst, tuples(1)._1, tuples(1)._2, radius)
        }

        (task1.join, task2.join)
      }
    }
  }

}
