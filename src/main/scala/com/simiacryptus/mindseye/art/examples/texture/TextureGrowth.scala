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

import java.awt.image.BufferedImage
import java.awt.{Font, Graphics2D}
import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.VGG16
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.eval.Trainable
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.mindseye.opt.Step
import com.simiacryptus.mindseye.util.ImageUtil
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner.withMonitoredJpg
import com.simiacryptus.sparkbook._
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


object TextureGrowth extends TextureGrowth with LocalRunner[Object] with NotebookRunner[Object]

class TextureGrowth extends ArtSetup[Object, TextureGrowth] {

  val styleUrl = "upload:Style"
  val initUrl: String = "50 + plasma * 0.5"
  val s3bucket: String = "test.deepartist.org"
  val minResolution = 200
  val maxResolution = 800
  val animationDelay = 1000
  val magnification = Array(4.0)

  override def indexStr = "103"

  override def description = <div>
    Paints a texture using a variety of resolution schedules, each with:
    <ol>
      <li>A single input image to define style</li>
      <li>Random noise initialization</li>
      <li>Standard VGG16 layers to define the style</li>
      <li>Operators to match content and constrain and enhance style</li>
    </ol>
    Demonstrates the effect of iteratively repainting while magnifying an image.
  </div>.toString.trim

  override def inputTimeoutSeconds = 3600

  override def postConfigure(log: NotebookOutput) = log.eval { () =>
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    () => {
      implicit val implicitLog = log
      if (Option(s3bucket).filter(!_.isEmpty).isDefined)
        log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
      log.onComplete(() => upload(log): Unit)
      log.out(log.jpg(ImageArtUtil.loadImage(log, styleUrl, (maxResolution * Math.sqrt(magnification.head)).toInt), "Input Style"))
      val renderedCanvases = new ArrayBuffer[() => BufferedImage]
      val registration = registerWithIndexGIF(renderedCanvases.map(_ ()).toList, delay = animationDelay)
      NotebookRunner.withMonitoredGif(() => {
        renderedCanvases.map(_ ())
      }.toList, delay = animationDelay) {
        try {
          val pipeline = VGG16.getVisionPipeline
          import scala.collection.JavaConverters._
          for (layer <- pipeline.getLayerList.asScala) {
            log.h1(layer.name())
            for (numberOfSteps <- List(1, 2, 5)) {
              log.h2(s"$numberOfSteps steps")
              val canvas = new RefAtomicReference[Tensor](null)
              renderedCanvases += (() => {
                val image = ImageUtil.resize(canvas.get().toImage, maxResolution)
                if (null == image) image else {
                  val graphics = image.getGraphics.asInstanceOf[Graphics2D]
                  graphics.setFont(new Font("Calibri", Font.BOLD, 24))
                  graphics.drawString(layer.name(), 10, 25)
                  graphics.drawString(s"$numberOfSteps steps", 10, image.getHeight - 10)
                  image
                }
              })
              withMonitoredJpg(() => Option(canvas.get()).map(_.toRgbImage).orNull) {
                var steps = 0
                Try {
                  log.subreport(s"$numberOfSteps steps", (sub: NotebookOutput) => {
                    paint(
                      contentUrl = styleUrl,
                      initUrl = initUrl,
                      canvas = canvas,
                      network = new VisualStyleNetwork(
                        styleLayers = List(layer),
                        styleModifiers = List(
                          new GramMatrixEnhancer(),
                          new MomentMatcher()
                        ),
                        styleUrls = List(styleUrl),
                        magnification = magnification
                      ), optimizer = new ImageOptimizer {
                        override val trainingMinutes: Int = 60 / numberOfSteps
                        override val trainingIterations: Int = 30 / numberOfSteps
                        override val maxRate = 1e9

                        override def onStepComplete(trainable: Trainable, currentPoint: Step): Boolean = {
                          steps = steps + 1
                          super.onStepComplete(trainable, currentPoint)
                        }
                      },
                      aspect = None,
                      resolutions = new GeometricSequence {
                        override val min: Double = minResolution
                        override val max: Double = maxResolution
                        override val steps = numberOfSteps
                      }.toStream.map(_.round.toDouble))(sub)
                    null
                  })
                }
                if (steps < 3 && !renderedCanvases.isEmpty) {
                  renderedCanvases.remove(renderedCanvases.size - 1)
                }
                uploadAsync(log)
              }(log)
            }
          }
          null
        } finally {
          registration.foreach(_.stop()(s3client, ec2client))
        }
      }
    }
  }()
}
