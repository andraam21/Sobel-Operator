import util.Pixel

import scala.annotation.tailrec

object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]


  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val lines = image.mkString.split("\n").toList
    val width_and_height = lines.tail.head.split(" ").map(_.toInt).toList

    def getPixelMatrix(line: List[Int], acc: List[List[Pixel]]): List[List[Pixel]] = {
      val pixel = Pixel(line(0), line(1), line(2))

      acc match {
        case Nil => List(List(pixel))
        case l :: ls =>  if (l.length % width_and_height.head == 0) (pixel :: Nil) :: acc else (pixel :: l) :: ls
      }
    }

    val matrix = lines.slice(3, lines.length).flatMap(_.split(" ").map(_.toInt)).grouped(3).toList
    val finalImage = matrix.foldRight(Nil: List[List[Pixel]])(getPixelMatrix)
    finalImage
  }

  def toStringPPM(image: Image): List[Char] = {
    val header = ("P3\n" + image.head.length + " " + image.length + "\n" + "255" + "\n").toList
    val pixels = image.flatMap(line => line.map(p => (p.red + " " + p.green + " " + p.blue + '\n')).toList).flatten
    val finalImage = header ++ pixels
    finalImage
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    val finalImage = image1 ++ image2
    finalImage
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    val first = image1.transpose
    val second = image2.transpose
    val finalImage = (first ++ second).transpose
    finalImage
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    degrees match {
      case 360 => image
      case 270 => image.reverse.transpose
      case 180 => image.transpose.reverse.transpose.reverse
      case 90 => image.transpose.reverse
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {

    def addMatrices(A: GrayscaleImage, B: GrayscaleImage): GrayscaleImage = {
      A.zip(B).map { case (rowA, rowB) =>
        rowA.zip(rowB).map { case (x, y) =>
          x.abs + y.abs
        }
      }
    }

    val gray = image.flatten(_.map(p => util.Util.toGrayScale(p)))
    val matrix = gray.grouped(image.head.length).toList.take(image.length)
    val conv = applyConvolution(matrix, gaussianBlurKernel)
    val Mx = applyConvolution(conv, Gx)
    val My = applyConvolution(conv, Gy)
    val sum = addMatrices(Mx, My).flatten
    val black = Pixel(0, 0, 0)
    val white = Pixel(255, 255, 255)
    val finalImage = sum.map(d => if (d < threshold) black else white).grouped(Mx.head.length).toList
    finalImage
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
    
    val neighbour = util.Util.getNeighbors(image, kernel.length / 2)
    val convolution = neighbour.map(_.map(_.zip(kernel).map { case (lineA, lineB) => lineA.zip(lineB).map {
                                                              case (x, y) => x * y }.sum }.sum))
    val finalImage = convolution.flatten.grouped(image.head.length - kernel.length + (kernel.length % 2)).toList
    finalImage
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {

    def generateNextRow(upperRow: List[Int]): List[Int] = {
      val sublist = (0 until size).map {
        case a if a == 0 => 1
        case a if a >= 1 => (upperRow(a) + upperRow(a - 1)) % m
      }.toList
      sublist
    }

    def pascalTriangle(n: Int): List[List[Int]] = {
      n match {
        case 0 => Nil
        case 1 => List(1 :: List.fill(size - 1)(0))
        case _ =>
          val triangle = pascalTriangle(n - 1)
          val nxt = generateNextRow(triangle.last)
          triangle :+ nxt
      }
    }

    def transform(pixel: Int, acc: List[List[Pixel]]): List[List[Pixel]] = {
      acc match {
        case Nil => List(List(funct(pixel)))
        case l :: ls => if (l.length % size == 0) (funct(-1) :: Nil) :: acc
                        else if(l.length >= ls.length) (funct(pixel) :: l) :: ls
                        else (funct(-1) :: l) :: ls
      }
    }

    val pascal = pascalTriangle(size)
    val finalImage = pascal.flatMap(_.map(p => p)).foldRight(Nil: List[List[Pixel]])(transform)
    finalImage
  }
}
