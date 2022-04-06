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

package com.simiacryptus.mindseye.art.examples.styled

import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.VGG16
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.NotebookRunner._
import com.simiacryptus.sparkbook.util.LocalRunner


object HighResStyleTransfer extends HighResStyleTransfer with LocalRunner[Object] with NotebookRunner[Object]

class HighResStyleTransfer extends ArtSetup[Object, HighResStyleTransfer] {

  val contentUrl = "upload:Content"
  val styleUrl = "upload:Style"
  val initUrl: String = "50 + noise * 0.5"
  val s3bucket: String = "test.deepartist.org"

  override def indexStr = "301"

  override def description = <div>
    Paints an image in the style of another using multiple resolution phases, each with tuned parameters.
    The result is a high resolution, high quality rendered painting.
  </div>.toString.trim

  override def inputTimeoutSeconds = 3600


  override def postConfigure(log: NotebookOutput) = log.eval { () =>
    () => {
      implicit val implicitLog = log
      implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
      // First, basic configuration so we publish to our s3 site
      if (Option(s3bucket).filter(!_.isEmpty).isDefined)
        log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
      log.onComplete(() => upload(log): Unit)
      // Fetch input images (user upload prompts) and display a rescaled copies
      log.p(log.jpg(ImageArtUtil.loadImage(log, styleUrl, (1200 * Math.sqrt(2)).toInt), "Input Style"))
      log.p(log.jpg(ImageArtUtil.loadImage(log, contentUrl, 1200), "Input Content"))
      val canvas = new RefAtomicReference[Tensor](null)
      // Execute the main process while registered with the site index
      val registration = registerWithIndexJPG(() => canvas.get())
      try {
        // Display an additional image inside the report itself
        withMonitoredJpg(() => canvas.get().toImage) {
          paint(
            contentUrl = contentUrl,
            initUrl = initUrl,
            canvas = canvas,
            network = new VisualStyleContentNetwork(
              styleLayers = List(
                VGG16.VGG16_1b1,
                VGG16.VGG16_1b2,
                VGG16.VGG16_1c1,
                VGG16.VGG16_1c2,
                VGG16.VGG16_1c3,
                VGG16.VGG16_1d1,
                VGG16.VGG16_1d2,
                VGG16.VGG16_1d3
              ),
              styleModifiers = List(
                new GramMatrixEnhancer().setMinMax(-5, 5),
                new MomentMatcher()
              ),
              styleUrls = List(styleUrl),
              contentLayers = List(
                VGG16.VGG16_1c1
              ),
              contentModifiers = List(
                new ContentMatcher().scale(1e1)
              ),
              magnification = Array(9.0)
            ) + new VisualStyleNetwork(
              styleLayers = List(
                VGG16.VGG16_0a
              ),
              styleModifiers = List(
                new GramMatrixEnhancer(),
                new MomentMatcher()
              ).map(_.scale(1e2)),
              styleUrls = List(contentUrl),
              magnification = Array(9.0)
            ), optimizer = new ImageOptimizer {
              override val trainingMinutes: Int = 60
              override val trainingIterations: Int = 20
              override val maxRate = 1e9
            }, aspect = None, resolutions = new GeometricSequence {
              override val min: Double = 200
              override val max: Double = 400
              override val steps = 2
            }.toStream.map(_.round.toDouble))
          paint(
            contentUrl = contentUrl,
            initUrl = initUrl,
            canvas = canvas,
            network = new VisualStyleContentNetwork(
              styleLayers = List(
                VGG16.VGG16_1a,
                VGG16.VGG16_1b1,
                VGG16.VGG16_1b2,
                VGG16.VGG16_1c1,
                VGG16.VGG16_1c2,
                VGG16.VGG16_1c3,
                VGG16.VGG16_1d1,
                VGG16.VGG16_1d2,
                VGG16.VGG16_1d3,
                VGG16.VGG16_1e1,
                VGG16.VGG16_1e2,
                VGG16.VGG16_1e3
              ),
              styleModifiers = List(
                new GramMatrixEnhancer().setMinMax(-2, 2),
                new MomentMatcher()
              ),
              styleUrls = List(styleUrl),
              contentLayers = List(
                VGG16.VGG16_1b2.prependAvgPool(2)
              ),
              contentModifiers = List(
                new ContentMatcher().scale(1e1)
              ),
              magnification = Array(4.0)
            ) + new VisualStyleNetwork(
              styleLayers = List(
                VGG16.VGG16_0a
              ),
              styleModifiers = List(
                new GramMatrixEnhancer(),
                new MomentMatcher()
              ).map(_.scale(1e2)),
              styleUrls = List(contentUrl),
              magnification = Array(4.0)
            ), optimizer = new ImageOptimizer {
              override val trainingMinutes: Int = 60
              override val trainingIterations: Int = 20
              override val maxRate = 1e9
            },
            aspect = None,
            resolutions = new GeometricSequence {
              override val min: Double = 600
              override val max: Double = 800
              override val steps = 2
            }.toStream.map(_.round.toDouble))
          paint(
            contentUrl = contentUrl,
            initUrl = initUrl,
            canvas = canvas,
            network = new VisualStyleContentNetwork(
              styleLayers = List(
                VGG16.VGG16_1a,
                VGG16.VGG16_1b1,
                VGG16.VGG16_1b2,
                VGG16.VGG16_1c1,
                VGG16.VGG16_1c2,
                VGG16.VGG16_1c3,
                VGG16.VGG16_1d1,
                VGG16.VGG16_1d2,
                VGG16.VGG16_1d3,
                VGG16.VGG16_1e1,
                VGG16.VGG16_1e2,
                VGG16.VGG16_1e3
              ),
              styleModifiers = List(
                new ChannelMeanMatcher(),
                new GramMatrixMatcher()
              ),
              styleUrls = List(styleUrl),
              contentLayers = List(
                VGG16.VGG16_1b2.prependAvgPool(4)
              ),
              contentModifiers = List(
                new ContentMatcher().scale(1e1)
              )
            ) + new VisualStyleNetwork(
              styleLayers = List(
                VGG16.VGG16_0a
              ),
              styleModifiers = List(
                new ChannelMeanMatcher(),
                new GramMatrixMatcher()
              ).map(_.scale(1e2)),
              styleUrls = List(contentUrl),
              magnification = Array(2.0)
            ),
            optimizer = new ImageOptimizer {
              override val trainingMinutes: Int = 90
              override val trainingIterations: Int = 20
              override val maxRate = 1e9
            },
            aspect = None,
            resolutions = new GeometricSequence {
              override val min: Double = 1024
              override val max: Double = 1600
              override val steps = 3
            }.toStream.map(_.round.toDouble))
          paint(
            contentUrl = contentUrl,
            initUrl = initUrl,
            canvas = canvas,
            network = new VisualStyleContentNetwork(
              styleLayers = List(
                VGG16.VGG16_1a,
                VGG16.VGG16_1b1,
                VGG16.VGG16_1b2,
                VGG16.VGG16_1c1,
                VGG16.VGG16_1c2,
                VGG16.VGG16_1c3,
                VGG16.VGG16_1d1,
                VGG16.VGG16_1d2,
                VGG16.VGG16_1d3
              ),
              styleModifiers = List(
                new ChannelMeanMatcher(),
                new GramMatrixMatcher()
              ),
              styleUrls = List(styleUrl),
              contentLayers = List(
                VGG16.VGG16_1b2.prependAvgPool(8).appendMaxPool(2)
              ),
              contentModifiers = List(
                new ContentMatcher().scale(1e1)
              )
            ),
            optimizer = new ImageOptimizer {
              override val trainingMinutes: Int = 180
              override val trainingIterations: Int = 20
              override val maxRate = 1e9
            },
            aspect = None,
            resolutions = new GeometricSequence {
              override val min: Double = 2400
              override val max: Double = 2400
              override val steps = 1
            }.toStream.map(_.round.toDouble))
        }
        null
      } finally {
        registration.foreach(_.stop()(s3client, ec2client))
      }
    }
  }()
}


object Test {}