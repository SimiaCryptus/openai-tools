/*
 * Copyright (c) 2020 by Andrew Charneski.
 *
 * The author licenses this file to you under the
 * Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance
 * with the License.  You may obtain a copy
 * of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.simiacryptus.mindseye.art.examples.zoomrotor

import java.awt.image.BufferedImage
import java.io.File
import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.{Layer, Tensor}
import com.simiacryptus.mindseye.layers.cudnn.ProductLayer
import com.simiacryptus.mindseye.opt.region.TrustRegion
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.NotebookRunner._
import com.simiacryptus.sparkbook.util.LocalRunner
import com.simiacryptus.util.Util
import javax.imageio.ImageIO

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object ZoomingRotor extends DebugZoomingRotor
  with LocalRunner[Object]
  with NotebookRunner[Object] {
  override def http_port: Int = 1080
  override val s3bucket: String = reportingBucket
}

class ZoomingRotor[U <: ZoomingRotor[U]] extends RotorArt[U] with ArtSource[U] {

  override val rotationalSegments = 6
  override val rotationalChannelPermutation: Array[Int] = Array(1, 2, 3)
  val border = 0.0
  val magnification = Array(4.0)
  val innerCoeff = 1e1
  val reportingBucket: String = "examples.deepartist.org"
  def s3bucket: String = reportingBucket
  val resolution: Int = 640
  val totalZoom: Double = 0.01
  val stepZoom: Double = 0.5
  val enhancementCoeff: Double = 0
  val splitLayers = true
  override def inputTimeoutSeconds = 3600
  override def className: String = "ZoomingRotor"
  override def indexStr = "202"

  override val styles = Array(
    "upload:Style"
  )

  override val keyframes = Array(
    "upload:Keyframes"
  )

  def getOptimizer()(implicit log: NotebookOutput): ImageOptimizer = {
    log.eval(() => {
      new ImageOptimizer {
        override val trainingMinutes: Int = 90
        override val trainingIterations: Int = 10
        override val maxRate = 1e9

        override def trustRegion(layer: Layer): TrustRegion = null

        override def renderingNetwork(dims: Seq[Int]) = getKaleidoscope(dims.toArray).head
      }
    })
  }

  def getStyle(innerMask: Tensor)(implicit log: NotebookOutput): VisualNetwork = {
    if (splitLayers) {
      log.eval(() => {
        val outerMask = innerMask.map(x => 1 - x)
        val wrapper: VisualStyleNetwork => VisualNetwork = if (innerCoeff > 0) (style: VisualStyleNetwork) => {
          style.withContent(
            contentLayers = List(
              VGG19.VGG19_0a
            ), contentModifiers = List(
              new ContentMatcher()
                .withMask(innerMask.addRef())
                .scale(innerCoeff)
            ))
        } else {
          x => x
        }
        wrapper(new VisualStyleNetwork(
          styleLayers = List(
            VGG19.VGG19_1b2,
            VGG19.VGG19_1c1,
            VGG19.VGG19_1c2,
            VGG19.VGG19_1c3,
            VGG19.VGG19_1c4
          ),
          styleModifiers = List(
            new MomentMatcher().scale(Math.pow(2, -enhancementCoeff * 2))
          ).map(_.withMask(outerMask.addRef())),
          styleUrls = styles,
          magnification = magnification,
          viewLayer = dims => getKaleidoscope(dims.toArray)
        )) + wrapper(new VisualStyleNetwork(
          styleLayers = List(
            VGG19.VGG19_1d1,
            VGG19.VGG19_1d2,
            VGG19.VGG19_1d3,
            VGG19.VGG19_1d4
          ),
          styleModifiers = List(
            new GramMatrixEnhancer().setMinMax(-5, 5).scale(Math.pow(2, enhancementCoeff * 2))
          ).map(_.withMask(outerMask.addRef())),
          styleUrls = styles,
          magnification = magnification,
          viewLayer = dims => getKaleidoscope(dims.toArray)
        ))
      })
    } else {
      log.eval(() => {
        val outerMask = innerMask.map(x => 1 - x)
        var style: VisualNetwork = new VisualStyleNetwork(
          styleLayers = List(
            VGG19.VGG19_1b2,
            VGG19.VGG19_1c1,
            VGG19.VGG19_1c2,
            VGG19.VGG19_1c3,
            VGG19.VGG19_1c4,
            VGG19.VGG19_1d1,
            VGG19.VGG19_1d2,
            VGG19.VGG19_1d3,
            VGG19.VGG19_1d4
          ),
          styleModifiers = List(
            new MomentMatcher().scale(Math.pow(2, -enhancementCoeff * 2)),
            new GramMatrixEnhancer().setMinMax(-5, 5).scale(Math.pow(2, enhancementCoeff * 2))
          ).map(_.withMask(outerMask.addRef())),
          styleUrls = styles,
          magnification = magnification,
          viewLayer = dims => getKaleidoscope(dims.toArray)
        )
        if (innerCoeff > 0) style = style.asInstanceOf[VisualStyleNetwork].withContent(
          contentLayers = List(
            VGG19.VGG19_0a
          ), contentModifiers = List(
            new ContentMatcher().withMask(innerMask.addRef()).scale(innerCoeff)
          ))
        style
      })
    }
  }

  override def description = <div>
    Creates a kaliedoscopic animation with:
    <ol>
      <li>Static images defining the start and end state</li>
      <li>Standard VGG19 layers</li>
      <li>Operators constraining and enhancing style</li>
      <li>Progressive scaling/zooming at constant resolution</li>
      <li>Kaleidoscopic view layer</li>
    </ol>
  </div>.toString.trim

  override def postConfigure(log: NotebookOutput) = {
    implicit val implicitLog = log
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    if (Option(s3bucket).filter(!_.isEmpty).isDefined) {
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
      log.onComplete(() => upload(log): Unit)
    }
    log.subreport[Null]("Styles", (sub: NotebookOutput) => {
      ImageArtUtil.loadImages(sub, styles.toList.asJava, (resolution * Math.sqrt(magnification.head)).toInt)
        .foreach(img => sub.p(sub.jpg(img, "Input Style")))
      null
    })
    val keyframes = log.subreport("KeyFrames", (sub: NotebookOutput) => {
      ImageArtUtil.loadImages(sub, this.keyframes.toList.asJava, (resolution * Math.sqrt(magnification.head)).toInt)
        .map(img => {
          sub.p(sub.jpg(img, "Keyframe"))
          "file:///" + sub.jpgFile(img).getAbsolutePath
        })
    })

    val flipbook: ArrayBuffer[Tensor] = new ArrayBuffer[Tensor]()
    val indexRegistration = registerWithIndexGIF(flipbook.map(_.addRef()).map(toRGB(_)).toList, 1000)
    try {
      val result = withMonitoredGif(() => flipbook.map(_.addRef()).map(toRGB(_)).toList, 1000) {
        renderTransitionLinearSeq(flipbook, keyframes ++ List(keyframes.head)).map(file => {
          Util.pathTo(log.getRoot, file).toString
        }).toArray
      }
      log.subreport("Animation", (sub: NotebookOutput) => {
        animate(result)(sub)
        null
      })
      log.json(result)
      result
    } finally {
      indexRegistration.foreach(_.stop()(s3client, ec2client))
    }
  }

  def animate(result: Seq[String])(implicit log: NotebookOutput) = {
    val image = ImageIO.read(new File(log.getRoot, result.head))
    val width = image.getWidth()
    val height = image.getHeight()
    val canvasId = "canvas_" + java.lang.Long.toHexString((Math.random() * 10000).toLong)
    log.addHeaderHtml("<script type=\"text/javascript\" src=\"https://simiacryptus.s3.us-west-2.amazonaws.com/descent-animator-opt.js\"></script>")
    log.out(<div>
      <canvas style="display: block" id={canvasId} width={width.toString} height={height.toString}></canvas>
      <script>
        {s"""var descent = new Descent($width, $height);
            |descent.stepZoom = ${stepZoom};
            |descent.animate(document.getElementById('$canvasId'),${result.map("'" + _ + "'").reduce(_ + "," + _)});""".stripMargin}
      </script>
    </div>.toString())
  }

  def renderTransitionLinearSeq(flipbook: ArrayBuffer[Tensor], imageUrls: Seq[String])(implicit log: NotebookOutput): Seq[File] = {
    if (imageUrls.size > 2) {
      renderTransitionLinearSeq(flipbook, imageUrls.take(2)) ++ renderTransitionLinearSeq(flipbook, imageUrls.drop(1))
    } else {
      val outerImage: BufferedImage = ImageArtUtil.loadImage(log, imageUrls(0), resolution)
      val innerImage: BufferedImage = ImageArtUtil.loadImage(log, imageUrls(1), resolution)
      log.p(log.jpg(outerImage, "Outer Image"))
      log.p(log.jpg(innerImage, "Inner Image"))
      log.subreport("Single Transition", (sub: NotebookOutput) => {
        renderTransitionAB(flipbook, outerImage, innerImage)(sub)
      })
    }
  }

  def renderTransitionAB(flipbook: ArrayBuffer[Tensor], outerImage: BufferedImage, innerImage: BufferedImage)(implicit log: NotebookOutput): Seq[File] = {
    val zoomFactor = 1.0 / (1.0 + stepZoom)
    val repeat: Int = log.eval(() => {
      (Math.log(totalZoom) / Math.log(zoomFactor)).ceil.toInt
    })
    var currentZoom = 1.0;
    var innerMask: Tensor = null
    var initUrl: String = null

    def zoomInner() = {
      currentZoom /= zoomFactor
      val progress = currentZoom / totalZoom
      val width = innerImage.getWidth()
      val height = innerImage.getHeight()
      val zoomedImage = zoom(Tensor.fromRGB(innerImage), 1 / progress)
      Option(innerMask).foreach(_.freeRef())
      innerMask = zoomMask(Array(width, height, 3), 1 / progress, (width * border).toInt)
      zoomedImage
    }

    def zoomOuter(previousOuterImage: BufferedImage, zoomedInner: Tensor) = {
      val zoomedOuter = zoom(Tensor.fromRGB(previousOuterImage), zoomFactor)
      val tensor = zoomedInner.mapCoords(c => {
        val mask = innerMask.get(c)
        zoomedInner.get(c) * mask + (1 - mask) * zoomedOuter.get(c)
      })
      initUrl = toFile(tensor.addRef())
      zoomedInner.freeRef()
      zoomedOuter.freeRef()
      tensor
    }

    flipbook += Tensor.fromRGB(outerImage)
    var zoomedInner = zoomInner()
    var currentImageTensor = zoomOuter(outerImage, zoomedInner.addRef())
    withMonitoredGif(() => (flipbook.map(_.addRef()) ++ Option(currentImageTensor).map(x => rotatedCanvas(x.addRef())).toList).map(toRGB(_)).toList, 1000) {
      try {
        List(
          log.jpgFile(outerImage)
        ) ++ (1 to repeat).map(_ => {
          log.p(log.jpg(toRGB(zoomedInner.mapCoords(c => zoomedInner.get(c) * innerMask.get(c))), "Zoomed Inner"));
          val finalCanvas = paint(currentImageTensor, innerMask.addRef())
          val url = log.jpgFile(finalCanvas.toRgbImage)
          zoomedInner.freeRef()
          zoomedInner = zoomInner()
          currentImageTensor = zoomOuter(finalCanvas.toRgbImage, zoomedInner.addRef())
          flipbook += finalCanvas
          url
        }).toList
      } finally {
        currentImageTensor = null
      }
    }
  }

  def paint(imageTensor: Tensor)(implicit log: NotebookOutput): Tensor = {
    try {
      withMonitoredJpg(() => toRGB(rotatedCanvas(imageTensor.addRef()))) {
        log.subreport("Painting", (sub: NotebookOutput) => {
          paint_aspectFn(
            contentUrl = toFile(imageTensor.addRef())(sub),
            initFn = _ => imageTensor.addRef(),
            canvas = new RefAtomicReference[Tensor](null),
            network = getStyle(new Tensor(imageTensor.getDimensions: _*))(sub),
            optimizer = getOptimizer()(sub),
            resolutions = List(resolution.toDouble),
            heightFn = Option(_ => imageTensor.getDimensions()(1))
          )(sub)
        })
      }(log)
      rotatedCanvas(imageTensor)
    } finally {
      uploadAsync(log)
    }
  }

  def paint(imageTensor: Tensor, innerMask: Tensor)(implicit log: NotebookOutput): Tensor = {
    try {
      withMonitoredJpg(() => toRGB(rotatedCanvas(imageTensor.addRef()))) {
        log.subreport("Painting", (sub: NotebookOutput) => {
          paint_aspectFn(
            contentUrl = toFile(imageTensor.addRef())(sub),
            initFn = _ => imageTensor.addRef(),
            canvas = new RefAtomicReference[Tensor](null),
            network = getStyle(innerMask)(sub),
            optimizer = getOptimizer()(sub),
            resolutions = List(resolution.toDouble),
            heightFn = Option(_ => imageTensor.getDimensions()(1))
          )(sub)
        })
      }(log)
      rotatedCanvas(imageTensor)
    } finally {
      uploadAsync(log)
    }
  }

  def rotatedCanvas(input: Tensor) = {
    if (input == null) {
      null
    } else {
      val viewLayer = getKaleidoscope(input.getDimensions).head
      val result = viewLayer.eval(input)
      viewLayer.freeRef()
      val data = result.getData
      result.freeRef()
      val tensor = data.get(0)
      data.freeRef()
      tensor
    }
  }

  def getKaleidoscopeMask(canvasDims: Array[Int], mask: Tensor) = {
    for (network <- getKaleidoscope(canvasDims)) yield {
      network.add(new ProductLayer(), network.getHead, network.constValue(mask)).freeRef()
      network
    }
  }

  def toRGB(tensor: Tensor) = {
    val image = tensor.toRgbImage
    tensor.freeRef()
    image
  }

  def toFile(tensor: Tensor)(implicit log: NotebookOutput) = {
    "file:///" + log.jpgFile(toRGB(tensor)).getAbsolutePath
  }

  def zoomMask(dims: Array[Int], factor: Double, border: Int): Tensor = {
    val tensor = new Tensor(dims: _*)
    val zoomedTensor = tensor.mapCoords(coord => {
      val coords = coord.getCoords()
      val x = coords(0)
      val y = coords(1)
      val distFromBorder = List(x, dims(0) - x, y, dims(1) - y).reduce(Math.min(_, _))
      Math.min(1, distFromBorder.toDouble / border)
    })
    tensor.freeRef()
    zoom(zoomedTensor, factor)
  }

  def zoom(tensor: Tensor, factor: Double): Tensor = {
    val width = tensor.getDimensions()(0)
    val height = tensor.getDimensions()(1)
    val zoomedTensor = tensor.mapCoords(coord => {
      val coords = coord.getCoords()
      val x = ((coords(0) - (width / 2)) * factor).toInt + (width / 2)
      val y = ((coords(1) - (height / 2)) * factor).toInt + (height / 2)
      if (x < 0 || x >= width || y < 0 || y >= height) {
        0
      } else {
        tensor.get(x, y, coords(2))
      }
    })
    tensor.freeRef()
    zoomedTensor
  }
}
