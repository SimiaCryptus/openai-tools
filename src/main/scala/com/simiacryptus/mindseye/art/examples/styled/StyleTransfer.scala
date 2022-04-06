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
import com.simiacryptus.sparkbook.NotebookRunner._
import com.simiacryptus.sparkbook._
import com.simiacryptus.sparkbook.util.LocalRunner


object StyleTransfer extends StyleTransfer with LocalRunner[Object] with NotebookRunner[Object]

class StyleTransfer extends ArtSetup[Object, StyleTransfer] {

  val contentUrl = "upload:Content"
  val styleUrl = "upload:Style"
  val initUrl: String = "50 + noise * 0.5"
  val s3bucket: String = "test.deepartist.org"
  val minResolution = 320
  val maxResolution = 1600
  val magnification = Array(2.0)
  val steps = 4

  override def indexStr = "301"

  override def description = <div>
    Paints an image in the style of another using:
    <ol>
      <li>Random noise initialization</li>
      <li>Standard VGG16 layers</li>
      <li>Operators to match content and constrain and enhance style</li>
      <li>Progressive resolution increase</li>
    </ol>
  </div>.toString.trim

  override def inputTimeoutSeconds = 3600


  override def postConfigure(log: NotebookOutput) = log.eval { () =>
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    () => {
      implicit val implicitLog = log
      // First, basic configuration so we publish to our s3 site
      if (Option(s3bucket).filter(!_.isEmpty).isDefined)
        log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
      log.onComplete(() => upload(log): Unit)
      // Fetch input images (user upload prompts) and display a rescaled copies
      log.p(log.jpg(ImageArtUtil.loadImage(log, styleUrl, (maxResolution * Math.sqrt(magnification.head)).toInt), "Input Style"))
      log.p(log.jpg(ImageArtUtil.loadImage(log, contentUrl, maxResolution), "Input Content"))
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
                // We select all the lower-level layers to achieve a good balance between speed and accuracy.
                VGG16.VGG16_0b,
                VGG16.VGG16_1a,
                VGG16.VGG16_1b1,
                VGG16.VGG16_1b2,
                VGG16.VGG16_1c1,
                VGG16.VGG16_1c2,
                VGG16.VGG16_1c3
              ),
              styleModifiers = List(
                // These two operators are a good combination for a vivid yet accurate style
                new GramMatrixEnhancer(),
                new MomentMatcher()
              ),
              styleUrls = List(styleUrl),
              contentLayers = List(
                // We use fewer layer to be a constraint, since the ContentMatcher operation defines
                // a stronger operation. Picking a mid-level layer ensures the match is somewhat
                // faithful to color, contains detail, and still accomidates local changes for style.
                VGG16.VGG16_1b2
              ),
              contentModifiers = List(
                // Standard mask matching operator
                new ContentMatcher()
              ),
              magnification = magnification
            ),
            optimizer = new ImageOptimizer {
              override val trainingMinutes: Int = 60
              override val trainingIterations: Int = 20
              override val maxRate = 1e9
            },
            aspect = None,
            resolutions = new GeometricSequence {
              override val min: Double = minResolution
              override val max: Double = maxResolution
              override val steps = StyleTransfer.this.steps
            }.toStream.map(_.round.toDouble))
        }
        null
      } finally {
        registration.foreach(_.stop()(s3client, ec2client))
      }
    }
  }()
}
