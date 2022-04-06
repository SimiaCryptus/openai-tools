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

import java.io.File
import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.examples.zoomrotor.ZoomingRotor
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.{Layer, Tensor}
import com.simiacryptus.mindseye.network.PipelineNetwork
import com.simiacryptus.mindseye.opt.region.TrustRegion
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.util.LocalRunner

object AutoZoomingRotor extends AutoZoomingRotorBase with LocalRunner[Object] with NotebookRunner[Object] {
  override def http_port: Int = 1081
}

abstract class AutoZoomingRotorBase extends RotorArt[AutoZoomingRotorBase] {

  override val s3bucket: String = "test.deepartist.org"
  override val rotationalChannelPermutation: Array[Int] = Array(2, 3, 1)
  override val rotationalSegments = 6
  val resolution: Int = 1024
  val band = 18
  val visionLayer: VGG19 = VGG19.VGG19_1d4

  override def indexStr = "202"

  override def inputTimeoutSeconds = 3600

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
    if (Option(s3bucket).filter(!_.isEmpty).isDefined) {
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
    }
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    log.onComplete(() => upload(log): Unit)

    val keyFrame_files = log.subreport("TextureTiledRotor",
      texture().postConfigure(_).toList.map(t => log.jpgFile(t.toImage)))

    //rotor.postConfigure(log)).toArray
    log.subreport("ZoomingRotorBase",
      animation(keyFrame_files).postConfigure(_))

  }

  override def className: String = "AutoZoomingRotor"

  def animation(keyFrame_files: Seq[File]) = {
    new ZoomingRotor {
      override val border: Double = 0.0
      override val magnification = Array(2.0)
      override val rotationalSegments = AutoZoomingRotorBase.this.rotationalSegments
      override val rotationalChannelPermutation: Array[Int] = AutoZoomingRotorBase.this.rotationalChannelPermutation
      override val styles: Array[String] = Array(
        ""
      )
      override val keyframes = keyFrame_files.map(_.toURI.toString).toArray
      override val resolution: Int = AutoZoomingRotorBase.this.resolution
      override val totalZoom: Double = 0.01
      override val stepZoom: Double = 0.5
      override val innerCoeff: Double = 0

      override def className: String = AutoZoomingRotorBase.this.className

      override val s3bucket: String = AutoZoomingRotorBase.this.s3bucket

      override def getOptimizer()(implicit log: NotebookOutput): ImageOptimizer = {
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

      override def getStyle(innerMask: Tensor)(implicit log: NotebookOutput): VisualNetwork = {
        log.eval(() => {
          val outerMask = innerMask.map(x => 1 - x)
          val style1: VisualStyleNetwork = AutoZoomingRotorBase.this.getStyle(dims => getKaleidoscope(dims.toArray))
          var withMask: VisualNetwork = new VisualStyleNetwork(
            styleLayers = style1.styleLayers,
            styleModifiers = style1.styleModifiers.map(_.withMask(outerMask.addRef())),
            styleUrls = style1.styleUrls,
            magnification = style1.magnification,
            viewLayer = dims => getKaleidoscope(dims.toArray)
          )
          if (innerCoeff > 0) withMask = withMask.asInstanceOf[VisualStyleNetwork].withContent(
            contentLayers = List(
              VGG19.VGG19_0a
            ), contentModifiers = List(
              new ContentMatcher().withMask(innerMask.addRef()).scale(innerCoeff)
            ))
          withMask
        })
      }
    }
  }

  def getStyle(viewLayer: Seq[Int] => List[PipelineNetwork])(implicit log: NotebookOutput): VisualStyleNetwork = {
    new VisualStyleNetwork(
      styleLayers = List(
        visionLayer
      ),
      styleModifiers = List(
        new SingleChannelEnhancer(band, band + 1)
      ),
      styleUrls = Seq(""),
      magnification = Array(1.0),
      viewLayer = viewLayer
    )
  }

  def texture() = {
    new TextureTiledRotor {

      override val minResolution: Int = 128
      override val maxResolution: Int = AutoZoomingRotorBase.this.resolution
      //override val steps: Int = 3
      override val iterations: Int = 15
      //override val magnification = Array(2.0)
      override val rotationalSegments = AutoZoomingRotorBase.this.rotationalSegments
      override val rotationalChannelPermutation: Array[Int] = AutoZoomingRotorBase.this.rotationalChannelPermutation

      override val s3bucket: String = AutoZoomingRotorBase.this.s3bucket

      override def className: String = AutoZoomingRotorBase.this.className

    }


  }
}
