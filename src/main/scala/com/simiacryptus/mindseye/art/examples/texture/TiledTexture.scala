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

package com.simiacryptus.mindseye.art.examples.texture

import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.ImageArtUtil._
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.mindseye.layers.java.{AffineImgViewLayer, ImgTileAssemblyLayer}
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner.withMonitoredJpg
import com.simiacryptus.sparkbook._
import com.simiacryptus.sparkbook.util.LocalRunner

object TiledTexture extends TiledTexture with LocalRunner[Object] with NotebookRunner[Object] {
  override val styleUrl: String = "file:///C:/Users/andre/Pictures/texture_sources/the-starry-night.jpg"
}

class TiledTexture extends ArtSetup[Object, TiledTexture] {

  val styleUrl = "upload:Style"
  val initUrl: String = "50 + noise * 0.5"
  val s3bucket: String = "test.deepartist.org"
  val minResolution = 120
  val maxResolution = 400
  val magnification = Array(3.0)
  val rowsAndCols = 2
  val steps = 2
  val aspectRatio = 1.0
  val min_padding = 64
  val max_padding = 256
  val border_factor = 1.0

  override def indexStr = "201"

  override def description = <div>
    Creates a simple tiled texture based on a style using:
    <ol>
      <li>Random plasma initialization</li>
      <li>Standard VGG19 layers</li>
      <li>Operators constraining and enhancing style</li>
      <li>Progressive resolution increase</li>
      <li>View layer to enforce tiling</li>
    </ol>
  </div>.toString.trim

  override def inputTimeoutSeconds = 3600


  override def postConfigure(log: NotebookOutput) = {
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    log.eval[() => Null](() => {
      () => {
        implicit val implicitLog = log
        // First, basic configuration so we publish to our s3 site
        if (Option(s3bucket).filter(!_.isEmpty).isDefined)
          log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
        log.onComplete(() => upload(log): Unit)
        // Fetch image (user upload prompt) and display a rescaled copy
        loadImages(log, styleUrl, (maxResolution * Math.sqrt(magnification.head)).toInt).foreach(img => log.p(log.jpg(img, "Input Style")))
        val canvas = new RefAtomicReference[Tensor](null)

        // Generates a pretiled image (e.g. 3x3) to display
        def tiledCanvas = {
          val input = canvas.get()
          if (null == input) input else {
            val layer = new ImgTileAssemblyLayer(rowsAndCols, rowsAndCols)
            val result = layer.eval((1 to (rowsAndCols * rowsAndCols)).map(_ => input.addRef()): _*)
            input.freeRef()
            layer.freeRef()
            val tensorList = result.getData
            result.freeRef()
            val tensor = tensorList.get(0)
            tensorList.freeRef()
            tensor
          }
        }

        // Tiling layer used by the optimization engine.
        // Expands the canvas by a small amount, using tile wrap to draw in the expanded boundary.
        def viewLayer(dims: Seq[Int]) = {
          val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
          val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
          val layer = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
          layer.setOffsetX(-paddingX / 2)
          layer.setOffsetY(-paddingY / 2)
          List(layer)
        }

        // Execute the main process while registered with the site index
        val registration = registerWithIndexJPG(() => tiledCanvas)
        try {
          // Display a pre-tiled image inside the report itself
          withMonitoredJpg(() => {
            val tensor = tiledCanvas
            val image = tensor.toImage
            tensor.freeRef()
            image
          }) {
            withMonitoredJpg(() => Option(canvas.get()).map(tensor => {
              val imgViewLayer = viewLayer(tensor.getDimensions).head
              val result = imgViewLayer.eval(tensor)
              imgViewLayer.freeRef()
              val tensorList = result.getData
              result.freeRef()
              val data = tensorList.get(0)
              tensorList.freeRef()
              val image = data.toRgbImage
              data.freeRef()
              image
            }).orNull) {
              // Display an additional, non-tiled image of the canvas
              withMonitoredJpg(() => Option(canvas.get()).map(tensor => {
                val image = tensor.toRgbImage
                tensor.freeRef()
                image
              }).orNull) {
                log.subreport("Painting", (sub: NotebookOutput) => {
                  paint(
                    contentUrl = initUrl,
                    initUrl = initUrl,
                    canvas = canvas.addRef(),
                    network = new VisualStyleNetwork(
                      styleLayers = List(
                        // We select all the lower-level layers to achieve a good balance between speed and accuracy.
                        VGG19.VGG19_0b,
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
                        // These two operators are a good combination for a vivid yet accurate style
                        new GramMatrixEnhancer(),
                        new MomentMatcher()
                      ),
                      styleUrls = Seq(styleUrl),
                      magnification = magnification,
                      viewLayer = viewLayer
                    ),
                    optimizer = new ImageOptimizer {
                      override val trainingMinutes: Int = 60
                      override val trainingIterations: Int = 15
                      override val maxRate = 1e9
                    },
                    aspect = Option(aspectRatio),
                    resolutions = new GeometricSequence {
                      override val min: Double = minResolution
                      override val max: Double = maxResolution
                      override val steps = TiledTexture.this.steps
                    }.toStream.map(_.round.toDouble)
                  )(sub)
                  null
                })
              }(log)
            }
          }
          null
        } finally {
          canvas.freeRef()
          registration.foreach(_.stop()(s3client, ec2client))
        }
        null
      }
    })()
  }
}
