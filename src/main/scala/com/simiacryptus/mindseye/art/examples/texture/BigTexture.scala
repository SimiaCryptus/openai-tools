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
import com.simiacryptus.mindseye.layers.java.AffineImgViewLayer
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.NotebookRunner.withMonitoredJpg
import com.simiacryptus.sparkbook.util.LocalRunner


object BigTexture extends BigTexture with LocalRunner[Object] with NotebookRunner[Object]

class BigTexture extends ArtSetup[Object, BigTexture] {

    val styleUrl = "http://test.deepartist.org/BigTexture/1d165554-f60e-41b8-ab41-4e730ed17d72/etc/58098b35-0203-40c6-b3c6-c860a882089a.jpg"
//    val styleUrl = "file:///C:/Users/andre/code/all-projects/report/BigTexture/556a080f-5ef7-4c58-bbdd-4bee36486502/etc/shutterstock_87165334.jpg"

  override def inputTimeoutSeconds = 0

//  val initUrl: String = "file:///C:/Users/andre/code/all-projects/report/BigTexture/7d8f3695-9b29-4c83-b7fd-83ebafd4bb8b/etc/image_4648be07568b7c0f.jpg"

//      val styleUrl = "upload:Style"
  val initUrl: String = "plasma"
//  val initUrl: String = "50 + noise * 0.5"
  //  override def inputTimeoutSeconds = 3600
  val s3bucket: String = "test.deepartist.org"
  val aspectRatio = 0.5774
  val minRes = 200

  override def indexStr = "201"

  override def description = <div>
    Creates a large texture based on a style using:
    <ol>
      <li>Random plasma initialization</li>
      <li>Standard VGG19 layers</li>
      <li>Operators constraining and enhancing style</li>
      <li>Progressive resolution increase</li>
      <li>View layer to enforce tiling</li>
    </ol>
  </div>.toString.trim

  override def postConfigure(log: NotebookOutput) = {
    implicit val implicitLog = log
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    // First, basic configuration so we publish to our s3 site
    if (Option(s3bucket).filter(!_.isEmpty).isDefined)
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
    log.onComplete(() => upload(log): Unit)
    // Fetch image (user upload prompt) and display a rescaled copy
    log.subreport("Styles", (sub: NotebookOutput) => {
      loadImages(sub, styleUrl, -1).foreach(img => sub.p(sub.jpg(img, "Input Style")))
    })
    val canvas = new RefAtomicReference[Tensor](null)

    // Tiling layer used by the optimization engine.
    // Expands the canvas by a small amount, using tile wrap to draw in the expanded boundary.
    val min_padding = 0
    val max_padding = 128
    val border_factor = 0.0

    def viewLayer(dims: Seq[Int]) = {
      val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
      val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
      val layer = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
      layer.setOffsetX(-paddingX / 2)
      layer.setOffsetY(-paddingY / 2)
      List(layer)
    }

    // Execute the main process while registered with the site index
    val registration = registerWithIndexJPG(() => canvas.get())
    try {
      withMonitoredJpg(() => Option(canvas.get()).map(tensor => {
        val image = tensor.toRgbImage
        tensor.freeRef()
        image
      }).orNull) {
        if(minRes < 800) paint(
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
              //new GramMatrixEnhancer().setMinMax(-.05, .05).scale(1e1),
              new MomentMatcher()
//              new GramMatrixMatcher()
            ),
            styleUrls = Seq(styleUrl),
            magnification = Array(1.0),
            viewLayer = viewLayer
          ),
          optimizer = new ImageOptimizer {
            override val trainingMinutes: Int = 60
            override val trainingIterations: Int = 50
            override val maxRate = 1e9
          },
          aspect = Option(aspectRatio),
          resolutions = new GeometricSequence {
            override val min: Double = minRes
            override val max: Double = 800
            override val steps = 3
          }.toStream.map(_.round.toDouble)
        )
        paint(
          contentUrl = initUrl,
          initUrl = initUrl,
          canvas = canvas.addRef(),
          network = new VisualStyleNetwork(
            styleLayers = List(
              //VGG19.VGG19_0b,
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
//              new GramMatrixEnhancer().setMinMax(-.05, .05).scale(5e1),
//              new MomentMatcher()
              new GramMatrixMatcher(),
              new ChannelMeanMatcher().scale(1e1)
            ),
            styleUrls = Seq(styleUrl),
            magnification = Array(1.0),
            viewLayer = viewLayer
          ),
          optimizer = new ImageOptimizer {
            override val trainingMinutes: Int = 360
            override val trainingIterations: Int = 15
            override val maxRate = 1e9
          },
          aspect = Option(aspectRatio),
          resolutions = new GeometricSequence {
            override val min: Double = Math.max(1200, minRes)
            override val max: Double = 4800
            override val steps = 3
          }.toStream.map(_.round.toDouble)
        )
      }
      null
    } finally {
      registration.foreach(_.stop()(s3client, ec2client))
      canvas.freeRef()
    }
  }
}
