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
import com.simiacryptus.mindseye.art.TiledTrainable
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.ImageArtUtil._
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.eval.Trainable
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.mindseye.network.PipelineNetwork
import com.simiacryptus.mindseye.util.ImageUtil
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.RefAtomicReference
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.NotebookRunner.withMonitoredJpg
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable


object BigTextureTiles extends BigTextureTiles with LocalRunner[Object] with NotebookRunner[Object]

class BigTextureTiles extends ArtSetup[Object, BigTextureTiles] with ImageTileProcessor with ArtworkStyleGalleries {

  //val styleUrls = "http://test.deepartist.org/BigTexture/1d165554-f60e-41b8-ab41-4e730ed17d72/etc/58098b35-0203-40c6-b3c6-c860a882089a.jpg"
  //val styleUrls = Array("file:///C:/Users/andre/Pictures/texture_sources/the-starry-night.jpg")
  val styleUrls = Array("file:///C:/Users/andre/Pictures/texture_sources/shutterstock_1065730331.jpg")
  //val styleUrls = "upload:Style"
//  val styleUrls = Array(CubismPortraits.name)

  //val initUrl: String = "file:///C:/Users/andre/code/all-projects/report/BigTexture/7d8f3695-9b29-4c83-b7fd-83ebafd4bb8b/etc/image_4648be07568b7c0f.jpg"
//  val initUrl: String = "file:///C:/Users/andre/code/all-projects/report/MultiStylized/998d64df-85ac-4a01-977e-d6f6c34c64fd/etc/image_724d7c58ec31c48f.jpg"
  //val initUrl: String = "50 + noise * 0.5"
//  val initUrl: String = "plasma"
//  val initUrl = "file:///C:/Users/andre/Pictures/Artistry/Owned/IMG_20170924_145214.jpg"
  //  val initUrl = "file:///E:/AI_Reports/ColorTransfer/d4af6e02-72d0-47a5-aa2c-db09481496fd/etc/df168685-64f3-42d4-9d1d-aecf8bc4e83a.jpg"
  val initUrl = "file:///C:/Users/andre/code/all-projects/report/ColorTransfer/d4af6e02-72d0-47a5-aa2c-db09481496fd/etc/df168685-64f3-42d4-9d1d-aecf8bc4e83a.jpg"

  override def inputTimeoutSeconds = 0
  //  override def inputTimeoutSeconds = 3600

//  val s3bucket: String = ""
  val s3bucket: String = "test.deepartist.org"

  //val aspectRatio = 0.5774 // Hex Tiling
//  val aspectRatio = 1 / 0.61803398875 // Golden Ratio
  val aspectRatio = 0.0

  val tile_size = 400
  val tile_padding = 64

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
    log.subreport("Input Images", (sub: NotebookOutput) => {
      val lowRes = styleGalleries_lowRes(styleUrls).filter(!styleUrls.contains(_))
      val nonGallery = styleUrls.filter(!lowRes.contains(_))
      sub.h1("Styles")
      if(nonGallery.nonEmpty) {
        loadImages(sub, nonGallery.toList.asJava, -1).foreach(img => sub.p(sub.jpg(img, "Input Style")))
      }
      if(lowRes.nonEmpty) {
        sub.h2("Low Res Galleries")
        loadImages(sub, lowRes.asJava, -1).foreach(img => sub.p(sub.jpg(img, "Input Style")))
      }
      val highRes = styleGalleries_highRes(styleUrls).filter(!styleUrls.contains(_)).filter(!lowRes.contains(_))
      if(highRes.nonEmpty) {
        sub.h2("High Res Galleries")
        loadImages(sub, highRes.asJava, -1).foreach(img => sub.p(sub.jpg(img, "Input Style")))
      }
      sub.h1("Initial Image")
      sub.p("Note - This is an illustration/upload only. It will be re-rendered.")
      loadImages(sub, List(initUrl).asJava, 1024).foreach(img => sub.p(sub.jpg(img, "Initial Image")))
    })
    val canvas = new RefAtomicReference[Tensor](null)

    // Tiling layer used by the optimization engine.
    // Expands the canvas by a small amount, using tile wrap to draw in the expanded boundary.
    val min_padding = 0
    val max_padding = 128
    val border_factor = 0.0

    def viewLayer(dims: Seq[Int]) = {
      //      val paddingX = Math.min(max_padding, Math.max(min_padding, dims(0) * border_factor)).toInt
      //      val paddingY = Math.min(max_padding, Math.max(min_padding, dims(1) * border_factor)).toInt
      //      val layer = new AffineImgViewLayer(dims(0) + paddingX, dims(1) + paddingY, true)
      //      layer.setOffsetX(-paddingX / 2)
      //      layer.setOffsetY(-paddingY / 2)
      //      List(layer)
      List(new PipelineNetwork(1))
    }

    // Execute the main process while registered with the site index
    val registration = registerWithIndexJPG(() => canvas.get())

    try {
      withMonitoredJpg(() => Option(canvas.get()).map(tensor => {
        val image = tensor.toRgbImage
        tensor.freeRef()
        image
      }).orNull) {
//        paint(
//          contentUrl = initUrl,
//          initUrl = initUrl,
//          canvas = canvas.addRef(),
//          network = new VisualStyleNetwork(
//            styleLayers = List(
//              VGG19.VGG19_1b1,
//              VGG19.VGG19_1b2,
//              VGG19.VGG19_1c1,
//              VGG19.VGG19_1c2,
//              VGG19.VGG19_1c3,
//              VGG19.VGG19_1c4,
//              VGG19.VGG19_1d1,
//              VGG19.VGG19_1d2,
//              VGG19.VGG19_1d3,
//              VGG19.VGG19_1d4
//            ),
//            styleModifiers = List(
//              new GramMatrixEnhancer().setMinMax(-.1, .1).scale(1e0),
////              new MomentMatcher()
//              new GramMatrixMatcher()
//            ),
//            styleUrls = styleGalleries_lowRes(styleUrls),
//            magnification = Array(1.0).flatMap(x=>new GeometricSequence {
//              override val min: Double = 0.9
//              override val max: Double = 1.1
//              override val steps = 4
//            }.toStream.map(_*x)),
//            viewLayer = viewLayer
//          ),
//          optimizer = new ImageOptimizer {
//            override val trainingMinutes: Int = 120
//            override val trainingIterations: Int = 50
//            override val maxRate = 1e9
//          },
//          aspect = Option(aspectRatio).filter(_>0),
//          resolutions = new GeometricSequence {
//            override val min: Double = 512
//            override val max: Double = 1024
//            override val steps = 2
//          }.toStream.map(_.round.toDouble)
//        )
        paintPerTile(
          canvasRef = canvas,
          style = (width: Int) => new VisualStyleNetwork(
            styleLayers = List(
//              VGG19.VGG19_1b1,
//              VGG19.VGG19_1b2,
              VGG19.VGG19_1c1,
              VGG19.VGG19_1c2,
              VGG19.VGG19_1c3,
              VGG19.VGG19_1c4
            ),
            styleModifiers = List(
              new GramMatrixEnhancer().setMinMax(-.01, .01).scale(1e1),
              new GramMatrixMatcher()
            ),
            styleUrls = styleGalleries_highRes(styleUrls),
            magnification = Array(width.toDouble / tile_size).flatMap(x=>new GeometricSequence {
              override val min: Double = 1.0
              override val max: Double = 1.0
              override val steps = 1
            }.toStream.map(_*x)),
            viewLayer = viewLayer
          ),
          resolutions = new GeometricSequence {
            override val min: Double = 1800
//            override val min: Double = 6400
            override val max: Double = 4200
            override val steps = 3
            //            override val steps = 2
          },
          optimizer = new ImageOptimizer {
            override val trainingMinutes: Int = 60
            override val trainingIterations: Int = 15
            override val maxRate = 1e9
          })

      }
      null
    } finally {
      registration.foreach(_.stop()(s3client, ec2client))
      canvas.freeRef()
    }
  }

  def paintPerTile(canvasRef: RefAtomicReference[Tensor], style: Int => VisualStyleNetwork, resolutions: GeometricSequence, optimizer: ImageOptimizer)(implicit log: NotebookOutput): Unit = {
    resolutions.toStream.map(_.round.toInt).foreach(res => {
      log.subreport(s"Resolution $res", (sub: NotebookOutput) => {

        initCanvas(canvasRef, res)(sub)
        tuneCanvas(canvasRef, tile_size, tile_padding)

        val cache = new mutable.HashMap[List[Int], (Tensor, Trainable)]()

        def getTrainer(dims: List[Int]) = {
          val (tile, styleTrainable) = cache.getOrElseUpdate(dims, {
            val tile = new Tensor(dims: _*)
            val styleTrainable: Trainable = stylePrepFn(
              contentUrl = initUrl,
              network = style(res),
              canvas = tile.addRef(),
              width = tile_size
            )(sub)
            (tile, styleTrainable)
          })
          (tile.addRef(), styleTrainable.addRef())
        }

        paintPerTile(canvasRef, tile_size, (tileInput, tileCanvasRef) => {
          val (tileCanvas, styleTrainable) = getTrainer(tileInput.getDimensions.toList)
          tileCanvas.set(tileInput)
          tileCanvasRef.set(tileCanvas.addRef())
          optimizer.optimize(tileCanvas, styleTrainable)(sub)
        })(sub)

      }: Unit)
    })
  }

  def initCanvas(canvasRef: RefAtomicReference[Tensor], res: Int)(implicit log: NotebookOutput): Unit = {
    val canvas = canvasRef.get()
    if (canvas == null) {
      canvasRef.set(getImageTensors(initUrl, log, res).head)
    } else {
      canvasRef.set(Tensor.fromRGB(ImageUtil.resize(canvas.toRgbImage, res, true)))
      canvas.freeRef()
    }
  }

  def tuneCanvas(canvasRef: RefAtomicReference[Tensor], tileSize: Int, padding: Int): Unit = {
    val canvas = canvasRef.get()
    val dims = canvas.getDimensions()
    val width = TiledTrainable.closestEvenSize(tileSize, padding, dims(0))
    val height = TiledTrainable.closestEvenSize(tileSize, padding, dims(1))
    canvasRef.set(Tensor.fromRGB(ImageUtil.resize(canvas.toRgbImage, width, height)))
    canvas.freeRef()
  }

}



