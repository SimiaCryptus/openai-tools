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

package com.simiacryptus.mindseye.art.examples

import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.TiledTrainable
import com.simiacryptus.mindseye.art.photo.affinity.{RasterAffinity, RelativeAffinity}
import com.simiacryptus.mindseye.art.photo.cuda.SmoothSolver_Cuda
import com.simiacryptus.mindseye.art.photo.topology.SearchRadiusTopology
import com.simiacryptus.mindseye.art.photo.{SmoothSolver, SmoothSolver_EJML}
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util._
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.mindseye.util.ImageUtil
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.util.LocalRunner


object PosterizeColors extends PosterizeColors with LocalRunner[Object] with NotebookRunner[Object] {
  override def http_port: Int = 1081
}

class PosterizeColors extends ArtSetup[Object, PosterizeColors] {

//  val contentUrl = "upload:Image"
  val contentUrl = "file:///C:/Users/andre/code/all-projects/report/ColorTransfer/d4af6e02-72d0-47a5-aa2c-db09481496fd/etc/424ae4ec-0e7e-4725-b1ef-6fb3849a9d1f.jpg"

  val useCuda = true
  val size = 320

  val s3bucket: String = ""
  override def inputTimeoutSeconds = 0
  //override def inputTimeoutSeconds = 600
  override def indexStr = "301"

  override def description = <div>
    High quality content smoothing via tiled spare connectivity matrix analysis
  </div>.toString.trim

  def solver: SmoothSolver = if(useCuda) new SmoothSolver_Cuda() else new SmoothSolver_EJML()
  override def postConfigure(log: NotebookOutput) = {
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    implicit val implicitLog = log
    if (Option(s3bucket).filter(!_.isEmpty).isDefined)
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
    log.onComplete(() => upload(log): Unit)
    var fullContent = ImageArtUtil.loadImageFile(contentUrl)
    //fullContent = Tensor.fromRGB(ImageUtil.resize(fullContent.toRgbImage, 1400, true))
    //log.p(log.jpg(fullContent.toRgbImage, "Input Content"))
    //val registration = registerWithIndexJPG(() => fullContent)

    try {

      val image = Tensor.fromRGB(ImageUtil.resize(fullContent.toRgbImage, size, true))
      log.p(log.jpg(image.toRgbImage, "Input Image"))

//      {
//        log.p("RGB")
//        val recolored = recolor(image.addRef())(image.addRef())
//        log.p(log.jpg(recolored.toRgbImage, "RGB Recolored"))
//        recolored.freeRef()
//      }
//
//      {
//        log.p("LAB")
//        val labImage = ColorTransforms.rgb2lab(image.addRef())
//        val recolored = ColorTransforms.lab2rgb(recolor(labImage.addRef())(labImage.addRef()))
//        log.p(log.jpg(recolored.toRgbImage, "LAB Recolored"))
//        recolored.freeRef()
//        labImage.freeRef()
//      }
//
//      {
//        log.p("LAB/RGB")
//        val labImage = ColorTransforms.rgb2lab(image.addRef())
//        val recolored = recolor(labImage.addRef())(image.addRef())
//        log.p(log.jpg(recolored.toRgbImage, "LAB Recolored"))
//        recolored.freeRef()
//        labImage.freeRef()
//      }

      val labImage = ColorTransforms.rgb2lab(image.addRef())
      val xyzImage = ColorTransforms.rgb2xyz(image.addRef())
      for(lambda <- List(3e-4, 1e-4, 3e-5)) {
        for(contrast <- List(30, 50, 80)) {
          for(mixing <- List(1e-3)) {
            try {
              val recoloring = Posterizor(
                lambda = lambda,
                contrast = contrast,
                mixing = mixing,
                useCuda = useCuda,
                neighborhoodSize = 3
              )
              log.p(recoloring.toString)

              {
                //log.p("LAB/XYZ")
                val recolored = ColorTransforms.xyz2rgb(recoloring(labImage.addRef(), xyzImage.addRef()))
                log.p(log.jpg(recolored.toRgbImage, "LAB Recolored"))
                recolored.freeRef()
              }

              System.gc()
            } catch {
              case e : Throwable => log.p(e.getMessage)
            }
          }
        }
      }
      labImage.freeRef()
      xyzImage.freeRef()

//      {
//        log.p("HSL")
//        val hslImage = ColorTransforms.rgb2hsl(image.addRef())
//        val recolored = ColorTransforms.hsl2rgb(recolor(hslImage.addRef())(hslImage.addRef()))
//        log.p(log.jpg(recolored.toRgbImage, "HSL Recolored"))
//        recolored.freeRef()
//        hslImage.freeRef()
//      }
//
//      {
//        log.p("HSV")
//        val hsvImage = ColorTransforms.rgb2hsv(image.addRef())
//        val recolored = ColorTransforms.hsv2rgb(recolor(hsvImage.addRef())(hsvImage.addRef()))
//        log.p(log.jpg(recolored.toRgbImage, "HSV Recolored"))
//        recolored.freeRef()
//        hsvImage.freeRef()
//      }
//
//      {
//        log.p("XYZ")
//        val xyzImage = ColorTransforms.rgb2xyz(image.addRef())
//        val recolored = ColorTransforms.xyz2rgb(recolor(xyzImage.addRef())(xyzImage.addRef()))
//        log.p(log.jpg(recolored.toRgbImage, "XYZ Recolored"))
//        recolored.freeRef()
//        xyzImage.freeRef()
//      }

      image.freeRef()
      null
    } finally {
      //registration.foreach(_.stop()(s3client, ec2client))
    }
  }

  def withRetry[T](n:Int=3)(fn: =>T):T = {
    try {
      fn
    } catch {
      case e : Throwable if(n>0) =>
        System.gc()
        withRetry(n-1)(fn)
    }
  }


}

