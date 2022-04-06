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

package com.simiacryptus.mindseye.art.examples.rotor

import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.{VGG16, VGG19}
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.HexMask
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.{Layer, Result, Tensor}
import com.simiacryptus.mindseye.layers.java.{AffineImgViewLayer, ImgTileAssemblyLayer}
import com.simiacryptus.mindseye.network.PipelineNetwork
import com.simiacryptus.mindseye.opt.region.TrustRegion
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner._
import com.simiacryptus.sparkbook._
import com.simiacryptus.sparkbook.util.LocalRunner

object TextureTiledRotor extends TextureTiledRotor with LocalRunner[Object] with NotebookRunner[Object] {
  override def http_port: Int = 1081
}


abstract class TextureTiledRotor extends RotorArt[TextureTiledRotor] {
  override val rotationalSegments = 6
  override val rotationalChannelPermutation: Array[Int] = Array(1, 2, 3)
  //  val styleUrl = "http://test.deepartist.org/TextureTiledRotor/124780fc-7d79-4698-985e-86ff3e8a6b4f/etc/1f5487ba-0d98-4602-8e3e-ac62f9740d53.jpg"
  //  val styleUrl = "http://test.deepartist.org/TextureTiledRotor/4662f8b9-9014-438d-9d11-c4684ea0d061/etc/a9ebe021-0319-44e4-9e0f-27cce3375547.jpg"
  //val styleUrl = "upload:Style"
  val styleUrl = "file:///C:/Users/andre/Pictures/texture_sources/shutterstock_542903440.jpg"

  val initUrls = Array(
    "plasma", "plasma", "50 + noise * 0.5", "50 + noise * 0.5"
    //    "http://test.deepartist.org/TextureTiledRotor/4662f8b9-9014-438d-9d11-c4684ea0d061/etc/image_7a53f141e1451361.jpg"
  )
  val reportingBucket = "examples.deepartist.org"

  def s3bucket: String = reportingBucket

  val minResolution = 100
  val midResolution = 320
  val maxResolution = 800
  val rowsAndCols = 2
  val aspectRatio = 1.732
  val min_padding = 126
  val max_padding = 256
  val border_factor = 0.5
  val iterations = 10
  val trainingMinutes = 90

  override def indexStr = "202"

  override def description = <div>
    Creates a tiled and rotationally symmetric texture based on a style using:
    <ol>
      <li>Random noise initialization</li>
      <li>Standard VGG19 layers</li>
      <li>Operators constraining and enhancing style</li>
      <li>Progressive resolution increase</li>
      <li>Kaleidoscopic view layer in addition to tiling layer</li>
    </ol>
  </div>.toString.trim

  override def inputTimeoutSeconds = 3600


  override def postConfigure(log: NotebookOutput): Seq[Tensor] = {
    implicit val implicitLog = log
    // First, basic configuration so we publish to our s3 site
    if (Option(s3bucket).filter(!_.isEmpty).isDefined) {
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
      log.onComplete(() => upload(log): Unit)
    }
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    log.subreport("Styles", (sub: NotebookOutput) => {
      ImageArtUtil.loadImages(sub, styleUrl, -1)
        .foreach(img => sub.p(sub.jpg(img, "Input Style")))
    })
    initUrls.map(initUrl => onePainting(initUrl))
  }

  def updateCanvas(canvasHolder: RefAtomicReference[Tensor], viewLayer_kaleidoscope: Seq[Int] => List[PipelineNetwork]) = {
    val canvas = canvasHolder.get()
    val dimensions = canvas.getDimensions
    val result = viewLayer_kaleidoscope(dimensions).head.eval(canvas)
    val data = result.getData
    result.freeRef()
    canvasHolder.set(data.get(0))
    data.freeRef()
  }

  def onePainting(initUrl: String)(implicit log: NotebookOutput, s3client: AmazonS3): Tensor = {
    val canvasHolder = new RefAtomicReference[Tensor](null)

    def rotatedCanvas = {
      val canvas = canvasHolder.get()
      if (null == canvas) canvas else {
        val viewLayer = getKaleidoscope(canvas.getDimensions).head
        Result.getData0(viewLayer.eval(canvas))
      }
    }

    def hexWrappedCanvas = {
      val canvas = rotatedCanvas
      if (null == canvas) canvas else {
        Result.getData0(HexMask.wrapLayer(canvas.getDimensions: _*).eval(canvas))
      }
    }

    // Generates a pretiled image (e.g. 3x3) to display
    def tiledCanvas(rowsAndCols: Int) = {
      val input = hexWrappedCanvas
      if (null == input) input else {
        val layer = new ImgTileAssemblyLayer(rowsAndCols, rowsAndCols)
        val result = layer.eval((1 to (rowsAndCols * rowsAndCols)).map(_ => input.addRef()): _*)
        layer.freeRef()
        input.freeRef()
        Result.getData0(result)
      }
    }

    // Kaleidoscope+Tiling layer used by the optimization engine.
    // Expands the canvas by a small amount, using tile wrap to draw in the expanded boundary.
    def viewLayer_kaleidoscope = (dims: Seq[Int]) => {
      for (rotor <- getKaleidoscope(dims.toArray)) yield {
        val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
        val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
        val tiling = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
        tiling.setOffsetX(-paddingX / 2)
        tiling.setOffsetY(-paddingY / 2)
        rotor.add(tiling).freeRef()
        rotor
      }
    }

    def viewLayer_kaleidoscope_hex = (dims: Seq[Int]) => {
      for (rotor <- getKaleidoscope(dims.toArray)) yield {
        val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
        val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
        val tiling = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
        tiling.setOffsetX(-paddingX / 2)
        tiling.setOffsetY(-paddingY / 2)
        rotor.add(HexMask.wrapLayer(dims: _*).addRef()).freeRef()
        rotor.add(tiling).freeRef()
        rotor
      }
    }

    def viewLayer_tiled = (dims: Seq[Int]) => {
      val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
      val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
      val tiling = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
      tiling.setOffsetX(-paddingX / 2)
      tiling.setOffsetY(-paddingY / 2)
      List(new PipelineNetwork(tiling))
    }

    // Execute the main process while registered with the site index
    val registration = registerWithIndexJPG(() => tiledCanvas(rowsAndCols))
    try {
      // Display a pre-tiled image
      withMonitoredJpg(() => {
        toImage(tiledCanvas(rowsAndCols))
      }) {
        // Display a pre-tiled hex image
        withMonitoredJpg(() => {
          toImage(hexMask(rotate90Degrees(centralBand(tiledCanvas(2), Math.cos(30 * (Math.PI / 180))))))
        }) {
          // Display an additional, non-tiled image of the canvas
          withMonitoredJpg(() => Option(rotatedCanvas).map(tensor => {
            toImage(tensor)
          }).orNull) {
            // Hex-Masked Image
            withMonitoredJpg(() => Option(rotatedCanvas).map(tensor => {
              toImage(hexMask(tensor))
            }).orNull) {
              // Hex-Minimal Image
              withMonitoredJpg(() => Option(rotatedCanvas).map(tensor => {
                toImage(centralBand(tensor, Math.cos(60 * (Math.PI / 180)) / Math.cos(30 * (Math.PI / 180))))
              }).orNull) {
                withMonitoredJpg(() => Option(rotatedCanvas).map(tensor => {
                  toImage(Result.getData0(HexMask.wrapLayer(tensor.getDimensions: _*).eval(tensor)))
                }).orNull) {
                  log.subreport("Painting", (sub: NotebookOutput) => {
                    paintingStage1(initUrl, canvasHolder, viewLayer_kaleidoscope_hex, minResolution, midResolution, 2)(sub)
                    //                     uploadAsync(log)
                    //              paintingStage1(initUrl, canvasHolder, viewLayer_kaleidoscope, minResolution, midResolution, 2)(sub)
                    //updateCanvas(canvasHolder, viewLayer_kaleidoscope)
                    paintingStage2(initUrl, canvasHolder, viewLayer_tiled, maxResolution)(sub)
                    null
                  })
                  uploadAsync(log)
                }(log)
              }(log)
            }(log)
          }(log)
        }
      }
      canvasHolder.get()
    } finally {
      registration.foreach(_.stop()(s3client, ec2client))
    }
  }

  private def centralBand(tensor: Tensor, aspect: Double) = {
    val Array(width, height, bands) = tensor.getDimensions
    centralPatch(tensor, width, (width * aspect).toInt)
  }

  private def centralPatch(tensor: Tensor, newWidth: Int, newHeight: Int) = {
    val Array(width, height, bands) = tensor.getDimensions
    val view = new AffineImgViewLayer(newWidth, newHeight)
    view.setOffsetX((width - newWidth) / 2)
    view.setOffsetY((height - newHeight) / 2)
    val tensor1 = Result.getData0(view.eval(tensor))
    view.freeRef();
    tensor1
  }

  private def rotate90Degrees(tensor: Tensor) = {
    val Array(width, height, bands) = tensor.getDimensions
    val view = new AffineImgViewLayer(height, width, true)
    //    view.setRotationCenterX(height / 2)
    //    view.setRotationCenterY(width / 2)
    view.setRotationRadians(Math.PI / 2)
    val tensor1 = Result.getData0(view.eval(tensor))
    view.freeRef();
    tensor1
  }

  private def hexMask(tensor: Tensor) = {
    val tensorCopy = tensor.copy()
    tensor.freeRef()
    tensorCopy.set(0, 0, 0, 255)
    tensorCopy.set(0, 0, 1, 255)
    tensorCopy.set(0, 0, 2, 255)
    Result.getData0(HexMask.maskLayer(tensorCopy.getDimensions: _*).eval(tensorCopy))
  }

  private def toImage(tensor: Tensor) = {
    val image = tensor.toRgbImage
    tensor.freeRef()
    image
  }

  def paintingStage2(canvasUrl: String, canvasHolder: RefAtomicReference[Tensor], viewLayer: Seq[Int] => List[PipelineNetwork], resolution: Int)(implicit log: NotebookOutput) = {
    paint(
      contentUrl = canvasUrl,
      initUrl = canvasUrl,
      canvas = canvasHolder.addRef(),
      network = {
        new VisualStyleNetwork(
          styleLayers = List(
            VGG16.VGG16_1a,
            VGG16.VGG16_1b1,
            VGG16.VGG16_1b2,
            VGG16.VGG16_1c1,
            VGG16.VGG16_1c2,
            VGG16.VGG16_1c3
            //        VGG19.VGG19_1a,
            //        VGG19.VGG19_1b1,
            //        VGG19.VGG19_1b2,
            //        VGG19.VGG19_1c1,
            //        VGG19.VGG19_1c2,
            //        VGG19.VGG19_1c3,
            //        VGG19.VGG19_1c4
          ),
          styleModifiers = List(
            new GramMatrixEnhancer().setMinMax(-10.0, 10.0).scale(1e1),
            //            new ChannelMeanMatcher(),
            new GramMatrixMatcher()
            //new SingleChannelEnhancer(130, 131)
          ),
          styleUrls = Seq(styleUrl),
          magnification = Array(8.0),
          viewLayer = viewLayer
        )
      },
      optimizer = new ImageOptimizer {
        override val trainingMinutes: Int = TextureTiledRotor.trainingMinutes
        override val trainingIterations: Int = iterations
        override val maxRate = 1e9

        override def trustRegion(layer: Layer): TrustRegion = null

        override def renderingNetwork(dims: Seq[Int]) = getKaleidoscope(dims.toArray).head
      },
      aspect = Option(aspectRatio),
      resolutions = new GeometricSequence {
        override val min: Double = resolution
        override val max: Double = resolution
        override val steps = 1
      }.toStream.map(_.round.toDouble))(log)
  }

  def paintingStage1(canvasUrl: String, canvasHolder: RefAtomicReference[Tensor], viewLayer: Seq[Int] => List[PipelineNetwork], minResolution: Int, midResolution: Int, paintingSteps: Int)(implicit log: NotebookOutput) = {
    paint(
      contentUrl = canvasUrl,
      initUrl = canvasUrl,
      canvas = canvasHolder.addRef(),
      network = {
        new VisualStyleNetwork(
          styleLayers = List(
            VGG19.VGG19_1a,
            VGG19.VGG19_1b1,
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
            new GramMatrixEnhancer().setMinMax(-10.0, 10.0),
            //        new ChannelMeanMatcher()
            new GramMatrixMatcher()
            //new SingleChannelEnhancer(130, 131)
          ),
          styleUrls = Seq(styleUrl),
          magnification = Array(8.0),
          viewLayer = viewLayer
        )
      },
      optimizer = new ImageOptimizer {
        override val trainingMinutes: Int = TextureTiledRotor.trainingMinutes
        override val trainingIterations: Int = iterations
        override val maxRate = 1e9

        override def trustRegion(layer: Layer): TrustRegion = null

        override def renderingNetwork(dims: Seq[Int]) = getKaleidoscope(dims.toArray).head
      },
      aspect = Option(aspectRatio),
      resolutions = new GeometricSequence {
        override val min: Double = minResolution
        override val max: Double = midResolution
        override val steps = paintingSteps
      }.toStream.map(_.round.toDouble))(log)
  }


}
