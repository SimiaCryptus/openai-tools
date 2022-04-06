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

import java.awt.image.BufferedImage
import java.awt.{Font, Graphics2D}
import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.registry.JobRegistration
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.mindseye.layers.java.{AffineImgViewLayer, ImgTileAssemblyLayer}
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.NotebookRunner._
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.mutable.ArrayBuffer


object NeuronTiledRotor extends NeuronTiledRotor with LocalRunner[Object] with NotebookRunner[Object]

class NeuronTiledRotor extends RotorArt[NeuronTiledRotor] {
  override val rotationalSegments = 6
  override val rotationalChannelPermutation: Array[Int] = Array(1, 2, 3)
  val initUrl: String = "50 + plasma * 0.5"
  val s3bucket: String = "test.deepartist.org"
  val minResolution = 128
  val maxResolution = 512
  val rowsAndCols = 2
  val steps = 2
  val aspectRatio = 0.5774
  val repeat = 1
  val min_padding = 8
  val max_padding = 32
  val border_factor = 0.125

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


  override def postConfigure(log: NotebookOutput) = {
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    log.eval[() => Unit](() => {
      () => {
        implicit val implicitLog = log
        // First, basic configuration so we publish to our s3 site
        if (Option(s3bucket).filter(!_.isEmpty).isDefined)
          log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
        log.onComplete(() => upload(log): Unit)
        for ((layer, toDim) <- List(
          //(VGG19.VGG19_1b2, 128),
          //(VGG19.VGG19_1c2, 255),
          //(VGG19.VGG19_1c4, 255),
          (VGG19.VGG19_1d4, 64)
          //(VGG19.VGG19_1e4, 512)
        )) {
          val fromDim = Math.max(0, toDim - 64)
          val animationDelay = 1000
          val renderedCanvases = new ArrayBuffer[() => BufferedImage]
          // Execute the main process while registered with the site index
          val registration = registerWithIndexGIF(renderedCanvases.filter(_ != null).map(_ ()).toList, delay = animationDelay)
          withMonitoredGif(() => renderedCanvases.filter(_ != null).map(_ ()).toList, delay = animationDelay) {
            try {
              log.subreport("Neurons in " + layer.name(), (sub: NotebookOutput) => {
                for ((list, page) <- (fromDim until toDim).toStream.grouped(8).zipWithIndex.toStream) {
                  sub.subreport("Page " + page, (sub2: NotebookOutput) => {
                    for (dimensionSelected <- list) {
                      sub2.h2(layer.name() + " " + dimensionSelected)
                      val size = renderedCanvases.size
                      (1 to repeat).map(_ => {
                        val image = test(layer, dimensionSelected)(sub2)
                        if (renderedCanvases.size > size) {
                          renderedCanvases(size) = () => image
                        } else {
                          renderedCanvases += (() => image)
                        }
                      })
                    }
                  })
                }
              })
              null
            } finally {
              registration.foreach(_.stop()(s3client, ec2client))
            }
          }
        }
      }
    })()
    null
  }

  def test(layer: VGG19, dimensionSelected: Int)(implicit log: NotebookOutput): BufferedImage = {
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    val registration: Option[JobRegistration[Tensor]] = None
    try {
      val canvas = new RefAtomicReference[Tensor](null)

      def rotatedCanvas = {
        var input = canvas.get()
        if (null == input) input else {
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

      // Generates a pretiled image (e.g. 3x3) to display
      def tiledCanvas = {
        var input = rotatedCanvas
        if (null == input) input else {
          val layer = new ImgTileAssemblyLayer(rowsAndCols, rowsAndCols)
          val result = layer.eval((1 to (rowsAndCols * rowsAndCols)).map(_ => input.addRef()): _*)
          layer.freeRef()
          input.freeRef()
          val data = result.getData
          result.freeRef()
          val tensor = data.get(0)
          data.freeRef()
          tensor
        }
      }

      // Kaleidoscope+Tiling layer used by the optimization engine.
      // Expands the canvas by a small amount, using tile wrap to draw in the expanded boundary.
      def viewLayer(dims: Seq[Int]) = {
        for(rotor <- getKaleidoscope(dims.toArray)) yield {
          val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
          val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
          val tiling = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
          tiling.setOffsetX(-paddingX / 2)
          tiling.setOffsetY(-paddingY / 2)
          rotor.add(tiling).freeRef()
          rotor
        }
      }

      // Display a pre-tiled image inside the report itself
      withMonitoredJpg(() => Option(rotatedCanvas).map(tensor => {
        val image = tensor.toRgbImage
        tensor.freeRef()
        image
      }).orNull) {
        log.subreport("Painting", (sub: NotebookOutput) => {
          withMonitoredJpg(() => {
            val tiledCanvas1 = tiledCanvas
            val toImage = tiledCanvas1.toImage
            tiledCanvas1.freeRef()
            toImage
          }) {
            paint(
              contentUrl = initUrl,
              initUrl = initUrl,
              canvas = canvas.addRef(),
              network = new VisualStyleNetwork(
                styleLayers = List(
                  layer
                ),
                styleModifiers = List(
                  new SingleChannelEnhancer(dimensionSelected, dimensionSelected + 1)
                ),
                styleUrls = Seq(""),
                viewLayer = viewLayer
              ),
              optimizer = new ImageOptimizer {
                override val trainingMinutes: Int = 90
                override val trainingIterations: Int = 15
                override val maxRate = 1e9

                //override def trustRegion(layer: Layer): TrustRegion = null

                override def renderingNetwork(dims: Seq[Int]) = getKaleidoscope(dims.toArray).head
              },
              aspect = Option(aspectRatio),
              resolutions = new GeometricSequence {
                override val min: Double = minResolution
                override val max: Double = maxResolution
                override val steps = NeuronTiledRotor.this.steps
              }.toStream.map(_.round.toDouble))(sub)
            null
          }(sub)
        })
        uploadAsync(log)
      }(log)

      val image = rotatedCanvas.toImage
      if (null == image) image else {
        val graphics = image.getGraphics.asInstanceOf[Graphics2D]
        graphics.setFont(new Font("Calibri", Font.BOLD, 24))
        graphics.drawString(layer.name() + " " + dimensionSelected, 10, 25)
        image
      }
    } finally {
      registration.foreach(_.stop()(s3client, ec2client))
    }
  }
}
