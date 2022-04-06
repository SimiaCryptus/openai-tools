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
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.{Layer, Tensor}
import com.simiacryptus.mindseye.opt.region.TrustRegion
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.util.LocalRunner


object AnimatedRotor extends AnimatedRotor with LocalRunner[Object] with NotebookRunner[Object]

class AnimatedRotor extends RotorArt[AnimatedRotor] {

  override val rotationalChannelPermutation: Array[Int] = Array(1, 2, 3)
  override val rotationalSegments: Int = 6
  val contentUrl = "upload:Content"
  val styleUrl = "upload:Style"
  val initUrl: String = "50 + noise * 0.5"
  val s3bucket: String = "test.deepartist.org"
  val minResolution = 200
  val maxResolution = 512
  val magnification = Array(2.0)
  val steps = 2
  val keyframes = 2

  override def indexStr = "303"

  override def description = <div>
    Paints a series of images, each to match the content of one while in the style of another using:
    <ol>
      <li>Random noise initialization</li>
      <li>Standard VGG19 layers</li>
      <li>Operators to match content and constrain and enhance style</li>
      <li>Progressive resolution increase</li>
      <li>Rotational symmerty constraint caused by a kaliedoscopic image layer</li>
      <li>A content seed image to guide the aspect ratio</li>
    </ol>
    The parameters for each frame are fixed, but due to the random initialization
    and loose constraints we can achive a dynamic effect.
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
      ImageArtUtil.loadImages(log, styleUrl, (maxResolution * Math.sqrt(magnification.head)).toInt).foreach(x => log.p(log.jpg(x, "Input Style")))
      log.p(log.jpg(ImageArtUtil.loadImage(log, contentUrl, maxResolution), "Input Content"))

      def frames = keyframes * 2 - 1

      val canvases = (1 to frames).map(_ => new RefAtomicReference[Tensor](null)).toList.toBuffer
      // Execute the main process while registered with the site index
      val registration = registerWithIndexGIF_Cyclic(canvases.map(_.get())
        .filter(_ != null)
        .map(t => {
          val kaleidoscope = getKaleidoscope(t.getDimensions).head
          val result = kaleidoscope.eval(t)
          kaleidoscope.freeRef()
          val tensorList = result.getData
          result.freeRef()
          val transformed = tensorList.get(0)
          tensorList.freeRef()
          transformed
        }).toList)
      try {
        animate(
          contentUrl = contentUrl,
          initUrl = initUrl,
          canvases = canvases,
          networks = (1 to frames).map(f => f.toDouble -> {
            new VisualStyleNetwork(
              styleLayers = List(
                // We select all the lower-level layers to achieve a good balance between speed and accuracy.
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
              // Our optimization network should be built with a kaliedoscope appended to the canvas
              viewLayer = dims => getKaleidoscope(dims.toArray)
            )
          }).toList.toBuffer,
          optimizer = new ImageOptimizer {
            override val trainingMinutes: Int = 60
            override val trainingIterations: Int = 10
            override val maxRate = 1e9

            // The canvas result should be processed by the kaliedoscope
            override def renderingNetwork(dims: Seq[Int]) = getKaleidoscope(dims.toArray).head

            // By default, we use a trust region that constrains the canvas pixel values from 0-256.
            // This conflicts with using a kaliedoscope, whose output is bounded and input may be out of that range.
            override def trustRegion(layer: Layer): TrustRegion = null
          },
          resolutions = new GeometricSequence {
            override val min: Double = minResolution
            override val max: Double = maxResolution
            override val steps = AnimatedRotor.this.steps
          }.toStream.map(_.round.toDouble),
          renderingFn = (dims: Seq[Int]) => getKaleidoscope(dims.toArray).head)
        null
      } finally {
        registration.foreach(_.stop()(s3client, ec2client))
      }
    }
  }()
}
