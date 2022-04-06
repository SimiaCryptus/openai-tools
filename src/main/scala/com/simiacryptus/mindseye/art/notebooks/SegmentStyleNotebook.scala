/*
 * Copyright (c) 2019 by Andrew Charneski.
 *
 * The author licenses this file to you under the
 * Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance
 * with the License.  You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.simiacryptus.mindseye.art.notebooks

import java.awt.Color._
import java.awt.image.BufferedImage
import java.awt.{Color, Desktop}
import java.io.File
import java.net.{URI, URL}
import java.util
import java.util.UUID
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference
import java.util.zip.ZipFile

import com.fasterxml.jackson.annotation.JsonIgnore
import com.simiacryptus.mindseye.art.models.VGG16
import com.simiacryptus.mindseye.art.ops.{ContentMatcher, GramMatrixEnhancer, MomentMatcher}
import com.simiacryptus.mindseye.art.photo.affinity.RasterAffinity.{adjust, degree}
import com.simiacryptus.mindseye.art.photo.affinity.RelativeAffinity
import com.simiacryptus.mindseye.art.photo.cuda.SmoothSolver_Cuda
import com.simiacryptus.mindseye.art.photo.topology.SearchRadiusTopology
import com.simiacryptus.mindseye.art.photo.{FastPhotoStyleTransfer, SmoothSolver}
import com.simiacryptus.mindseye.art.util._
import com.simiacryptus.mindseye.eval.Trainable
import com.simiacryptus.mindseye.lang.cudnn.CudaSettings
import com.simiacryptus.mindseye.lang.{Coordinate, Tensor}
import com.simiacryptus.mindseye.opt.Step
import com.simiacryptus.mindseye.util.ImageUtil
import com.simiacryptus.notebook.{NotebookOutput, NullNotebookOutput}
import com.simiacryptus.sparkbook.util.Java8Util._
import com.simiacryptus.util.{FastRandom, Util}
import javax.imageio.ImageIO

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object SegmentStyleNotebook {

  @JsonIgnore lazy val fastPhotoStyleTransfer = FastPhotoStyleTransfer.fromZip(new ZipFile(Util.cacheFile(new URI(
    "https://simiacryptus.s3-us-west-2.amazonaws.com/photo_wct.zip"))))

  def main(args: Array[String]): Unit = {

    val contentUrl = "file:///C:/Users/andre/Pictures/Personal/DSC_0386.JPG"
    val maskUrl = "file:///C:/Users/andre/Pictures/Personal/DSC_0386_mask.jpg"
    val styleUrl_background = "file:///C:/Users/andre/Pictures/texture_sources/shutterstock_1073629553.jpg"
    val styleUrl_foreground = "file:///C:/Users/andre/Pictures/texture_sources/shutterstock_240121861.jpg"
    val result_location = "C:/Users/andre/Pictures/"
    val initialResolution = 600
    val contentCoeff = 5e0
    var magnification = Array(8.0)

    //    val startServerThreadThread = new Thread(() => {
    //      polynote.Main.main(Array.empty)
    //      println("polynote.Main exit")
    //    })
    //    startServerThreadThread.start()

    val Seq(foreground: Tensor, background: Tensor) = loadMasks(
      loadImage(contentUrl, initialResolution), maskUrl, RED, GREEN)

    magnification = Array(32.0)
    implicit val implicitLog = new NullNotebookOutput()
    implicit val executionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
    val visualNetwork = new VisualStyleContentNetwork(
      styleLayers = List(
        VGG16.VGG16_1a,
        VGG16.VGG16_1b2,
        VGG16.VGG16_1c3
      ),
      styleModifiers = List(
        new GramMatrixEnhancer().setMinMax(-.125, .125),
        new MomentMatcher()
      ).map(_.withMask(foreground)),
      styleUrls = List(styleUrl_foreground),
      magnification = magnification
    ) + new VisualStyleContentNetwork(
      styleLayers = List(
        VGG16.VGG16_1a,
        VGG16.VGG16_1b2,
        VGG16.VGG16_1c3,
        VGG16.VGG16_1d3
      ),
      styleModifiers = List(
        new GramMatrixEnhancer().setMinMax(-.5, 5),
        new MomentMatcher()
      ).map(_.withMask(background)),
      styleUrls = List(styleUrl_background),
      contentLayers = List(
        VGG16.VGG16_1b2
      ),
      contentModifiers = List(
        new ContentMatcher().scale(contentCoeff)
      ).map(_.withMask(foreground)),
      magnification = magnification
    )

    val history = new ArrayBuffer[(Long, Long, Double, Double)]()
    val canvasRef = new AtomicReference[Tensor](null)

    val optimizer = new ImageOptimizer {
      override val trainingMinutes = 60
      override val trainingIterations = 20
      override val maxRate = 1e9

      override def onStepComplete(trainable: Trainable, currentPoint: Step): Boolean = {
        history ++= List((
          currentPoint.iteration,
          currentPoint.time,
          currentPoint.point.getMean,
          currentPoint.point.rate))
        super.onStepComplete(trainable, currentPoint)
      }

      override def onComplete()(implicit log: NotebookOutput): Unit = {
      }
    }

    val resolutions = new GeometricSequence {
      override val min = 400.0
      override val max = 800.0
      override val steps = 3
    }.toStream.map(_.round.toDouble)

    val paintingFuture = Future {
      for (width <- resolutions) {
        //log.h1("Resolution " + width)

        var contentImage = loadImage(contentUrl, width.toInt)
        val contentTensor = if (null == contentImage) {
          new Tensor(width.toInt, width.toInt, 3).map((x: Double) => FastRandom.INSTANCE.random())
        } else {
          Tensor.fromRGB(contentImage)
        }
        if (null == contentImage) contentImage = contentTensor.toImage

        require(null != canvasRef)
        val prevCanvas = canvasRef.get()
        val currentCanvas = {
          if (null == prevCanvas) {
            smoother(contentTensor)(wct(content = contentTensor,
              style = Tensor.fromRGB(loadImage(styleUrl_background, (initialResolution * Math.sqrt(magnification.head)).toInt)),
              mask = MomentMatcher.toMask(resize(background, contentTensor.getDimensions()))))
              .map((x: Double) => if (java.lang.Double.isFinite(x)) x else 0)
          } else {
            Tensor.fromRGB(ImageUtil.resize(prevCanvas.toRgbImage,
              if (null == contentImage) width.toInt else contentImage.getWidth,
              if (null == contentImage) width.toInt else contentImage.getHeight))
          }
        }
        canvasRef.set(currentCanvas)

        CudaSettings.INSTANCE().setDefaultPrecision(visualNetwork.precision)
        val trainable = visualNetwork.apply(currentCanvas, contentTensor)
        ArtUtil.resetPrecision(trainable.addRef().asInstanceOf[Trainable], visualNetwork.precision)
        optimizer.optimize(currentCanvas, trainable)
      }
    }

    Await.ready(paintingFuture, Duration.Inf)
    val outFile = new File(new File(result_location), UUID.randomUUID() + ".jpg")
    ImageIO.write(canvasRef.get().toImage, "jpg", outFile)
    Desktop.getDesktop.open(outFile)
  }

  def wct(content: Tensor, style: Tensor, mask: Tensor) = {
    val wctRestyled = fastPhotoStyleTransfer.photoWCT(style, content.addRef(), mask.doubleStream().average().getAsDouble, 1.0)
    maskedDelta(mask, content, wctRestyled)
  }

  def maskedDelta(mask: Tensor, base: Tensor, changed: Tensor) = {
    changed.mapCoords((c: Coordinate) => {
      val bg = mask.get(c)
      if (bg == 1) changed.get(c)
      else base.get(c)
    })
  }

  def smoother(content: Tensor) = {
    val topology = new SearchRadiusTopology(content)
    topology.setSelfRef(true)
    //.setVerbose(true)
    var affinity = new RelativeAffinity(content, topology)
    affinity.setContrast(10)
    affinity.setGraphPower1(2)
    affinity.setMixing(0.2)
    affinity.wrap((graphEdges: util.List[Array[Int]], innerResult: util.List[Array[Double]]) => adjust(graphEdges, innerResult, degree(innerResult), 0.5))
    solver.solve(topology, affinity, 1e-4)
  }

  def solver: SmoothSolver = new SmoothSolver_Cuda()

  def loadImage(file: String, width: Int): BufferedImage = {
    resize(loadTensor(file), width).toImage
  }

  def resize(foreground: Tensor, dims: Array[Int]) = {
    Tensor.fromRGB(ImageUtil.resize(foreground.toImage, dims(0), dims(1)))
  }

  def loadMasks(contentImage: BufferedImage, maskUrl: String, colors: Color*) = {
    val maskTensor = resize(loadTensor(maskUrl), contentImage.getWidth)
    val tensor = Tensor.fromRGB(contentImage)
    try {
      for (clr <- 0 until colors.size) yield {
        tensor.mapCoords((coordinate: Coordinate) => {
          val Array(x, y, c) = coordinate.getCoords()
          val pixelColor = maskTensor.getPixel(x, y)
          val closestColor = colors.zipWithIndex.sortBy(x => dist(x._1, pixelColor)).head
          if (closestColor._2 == clr) {
            tensor.get(coordinate)
          } else {
            0.0
          }
        })
      }
    } finally {
      maskTensor.freeRef()
      tensor.freeRef()
    }
  }

  def resize(tensor: Tensor, width: Int) = {
    Tensor.fromRGB(ImageUtil.resize(tensor.toImage, width, true))
  }

  def loadTensor(file: String) = {
    try {
      val read = ImageIO.read(new URL(file.toString))
      if (null == read) throw new IllegalArgumentException("Error reading " + file)
      Tensor.fromRGB(read)
    } catch {
      case e: Throwable =>
        throw new RuntimeException("Error reading " + file, e)
    }
  }

  def dist(color: Color, x: Seq[Double]) = {
    List(
      color.getRed - x(2).doubleValue(),
      color.getGreen - x(1).doubleValue(),
      color.getBlue - x(0).doubleValue()
    ).map(x => x * x).sum
  }
}
