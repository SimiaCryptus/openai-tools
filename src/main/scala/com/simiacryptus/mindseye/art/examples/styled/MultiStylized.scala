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

import java.awt.image.BufferedImage
import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.libs.LibraryNotebook
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.ImageArtUtil._
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.eval.Trainable
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.mindseye.layers.java.AffineImgViewLayer
import com.simiacryptus.mindseye.util.ImageUtil
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable


object MultiStylized extends MultiStylized with LocalRunner[Object] with NotebookRunner[Object]

class MultiStylized extends ArtSetup[Object, MultiStylized] with ArtworkStyleGalleries {

  //val styleUrls = "http://test.deepartist.org/BigTexture/1d165554-f60e-41b8-ab41-4e730ed17d72/etc/58098b35-0203-40c6-b3c6-c860a882089a.jpg"
//  val styleUrls = Array("file:///C:/Users/andre/code/all-projects/report/BigTexture/556a080f-5ef7-4c58-bbdd-4bee36486502/etc/shutterstock_87165334.jpg")
  //val styleUrls = "upload:Style"
  val styleUrls = Array(
    CubismPortraits.name
//    RealismLandscapes.name
//    "file:///C:/Users/andre/Pictures/texture_sources/the-starry-night.jpg"
  )

//  val contentUrl = "file:///C:/Users/andre/Pictures/Artistry/chimps/son_at_work.jpg"
  val contentUrl = "file:///C:/Users/andre/Downloads/20211010_124416_02.jpg"

  //val initUrl: String = "file:///C:/Users/andre/code/all-projects/report/BigTexture/7d8f3695-9b29-4c83-b7fd-83ebafd4bb8b/etc/image_4648be07568b7c0f.jpg"
  //val initUrl: String = "file:///C:/Users/andre/code/all-projects/report/BigTexture/faef1a35-a7ee-49a3-9f47-31380da7b5cc/etc/image_2ae03bd5518bf032.jpg"
  //val initUrl: String = "50 + noise * 0.5"
  val initUrls = Array(
    "50 + noise * 0.5",
    "plasma",
    "50 + noise * 0.5",
    "plasma"
  )

  val resolutions = GeometricSequenceJson(min = 256, max = 1024, steps = 3)
  val optimizer = ImageOptimizerJson(trainingMinutes = 120, trainingIterations = 50, maxRate = 1e9)
  val styleLayers = new VisionLayerListJson(
    //VGG19.VGG19_0b,
    //VGG19.VGG19_1a,
    VGG19.VGG19_1b1,
    VGG19.VGG19_1b2,
    VGG19.VGG19_1c1,
//    VGG19.VGG19_1c2,
    VGG19.VGG19_1c3,
    VGG19.VGG19_1c4,
    VGG19.VGG19_1d1,
    VGG19.VGG19_1d2,
    VGG19.VGG19_1d3,
    VGG19.VGG19_1d4
  )

  val contentLayers = new VisionLayerListJson(
//    VGG19.VGG19_0b,
//    VGG19.VGG19_1a,
//    VGG19.VGG19_1b1,
//    VGG19.VGG19_1b2,
//    VGG19.VGG19_1c1,
    VGG19.VGG19_1c2,
//    VGG19.VGG19_1c3,
//    VGG19.VGG19_1c4,
//    VGG19.VGG19_1d1,
//    VGG19.VGG19_1d2,
//    VGG19.VGG19_1d3,
//    VGG19.VGG19_1d4
  )

  override def inputTimeoutSeconds = 0
//    override def inputTimeoutSeconds = 3600

  //val s3bucket: String = "test.deepartist.org"
  val s3bucket: String = ""

  val contentCoeff = 1e1
  val min_padding = 0
  val max_padding = 128
  val border_factor = 0.0
  override def indexStr = "201"

  override def description = <div>
    Creates several renderings using the same style and content matching parameters.
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
      val lowRes = styleGalleries_lowRes(styleUrls).filter(!styleUrls.contains(_))
      val nonGallery = styleUrls.filter(styleGalleries_lowRes(styleUrls).contains(_))
      sub.h1("Styles")
      if (nonGallery.nonEmpty) {
        loadImages(sub, nonGallery.toList.asJava, -1).foreach(img => sub.p(sub.jpg(img, "Input Style")))
      }
      if (lowRes.nonEmpty) {
        sub.h2("Low Res Galleries")
        loadImages(sub, lowRes.asJava, -1).foreach(img => sub.p(sub.jpg(img, "Input Style")))
      }
    })
    val contentImage = log.subreport("Content", (sub: NotebookOutput) => {
      val contentImage = loadImages(sub, List(contentUrl).asJava, -1).head
      sub.p(sub.jpg(contentImage, "Input Style"))
      contentImage
    })
    val canvases = initUrls.map(_ => new RefAtomicReference[Tensor](null))

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
    val registrations = for (canvas <- canvases) yield {
      registerWithIndexJPG(() => canvas.get())
    }

    try {
      val contentNetwork = new VisualStyleNetwork(
        styleLayers = styleLayers.getLayerList(),
        styleModifiers = List(
          // These two operators are a good combination for a vivid yet accurate style
          //new GramMatrixEnhancer().setMinMax(-.05, .05).scale(1e1),
          new MomentMatcher()
          //              new GramMatrixMatcher()
        ),
        styleUrls = styleGalleries_lowRes(styleUrls),
        magnification = Array(1.0),
        viewLayer = viewLayer
      ).withContent(contentLayers.getLayerList(),
        List(new ContentMatcher().scale(contentCoeff))
      )
      multiPaint(
        canvasRefs = canvases,
        style = (width: Int) => contentNetwork,
        resolutions = resolutions,
        optimizer = optimizer, contentImage)
      null
    } finally {
      for (registration <- registrations) {
        registration.foreach(_.stop()(s3client, ec2client))
      }
      for (canvas <- canvases) {
        canvas.freeRef()
      }
    }
  }

  def multiPaint(canvasRefs: Seq[RefAtomicReference[Tensor]], style: Int => VisualNetwork, resolutions: GeometricSequence, optimizer: ImageOptimizer, contentImage:BufferedImage)(implicit log: NotebookOutput): Unit = {
    resolutions.toStream.map(_.round.toInt).foreach(width => {
      val height = (width.toDouble * contentImage.getHeight.toDouble / contentImage.getWidth.toDouble).floor.toInt
      log.subreport(s"Resolution: $width x $height", (sub: NotebookOutput) => {

        val cache = new mutable.HashMap[List[Int], (Tensor, Trainable)]()

        def getTrainer(dims: List[Int]) = {
          val (tile, styleTrainable) = cache.getOrElseUpdate(dims, {
            val canvas = new Tensor(dims: _*)
            val styleTrainable: Trainable = stylePrepFn(
              contentTensor = Tensor.fromRGB(ImageUtil.resize(contentImage, width, height)),
              network = style(width),
              canvas = canvas.addRef()
            )(sub)
            (canvas, styleTrainable)
          })
          (tile.addRef(), styleTrainable.addRef())
        }

        for ((canvasRef, initUrl) <- canvasRefs.zip(initUrls)) {
          initCanvas(canvasRef = canvasRef.addRef(), width = width, height = height, initUrl = initUrl)(sub)
          val content = canvasRef.get()
          val (trainerCanvas, styleTrainable) = getTrainer(content.getDimensions.toList)
          trainerCanvas.set(content.addRef())
          canvasRef.set(trainerCanvas.addRef())
          optimizer.optimize(trainerCanvas.addRef(), styleTrainable)(sub)
          content.set(trainerCanvas)
          canvasRef.set(content)
        }
      }: Unit)
    })
  }

  def initCanvas(canvasRef: RefAtomicReference[Tensor], width: Int, height: Int, initUrl: String)(implicit log: NotebookOutput): Unit = {
    val canvas = canvasRef.get()
    if (canvas == null) {
      if (initUrl.contains("://")) {
        canvasRef.set(getImageTensors(initUrl, log, width).head)
      } else {
        canvasRef.set(getImageTensors(initUrl, log, width, height).head)
      }
    } else {
      canvasRef.set(Tensor.fromRGB(ImageUtil.resize(canvas.toRgbImage, width, height)))
      canvas.freeRef()
    }
    canvasRef.freeRef()
  }


}


