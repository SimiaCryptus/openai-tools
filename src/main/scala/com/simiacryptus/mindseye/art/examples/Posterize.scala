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

object Posterize extends Posterize with LocalRunner[Object] with NotebookRunner[Object] {
  override def http_port: Int = 1081
}

class Posterize extends ArtSetup[Object, Posterize] {

//  val contentUrl = "upload:Image"
  val contentUrl = "file:///C:/Users/andre/Pictures/Johns Pics/IMG_20160312_122220887.jpg"
  val s3bucket: String = ""
  val useCuda = true
  val tile_size = 400
  val tile_padding = 64
  val lambda = 1e-4
  val contrast = 50
  val mixing = 0.1
  val freshRecoloring = true

  override def indexStr = "301"

  override def description = <div>
    High quality content smoothing via tiled spare connectivity matrix analysis
  </div>.toString.trim

  override def inputTimeoutSeconds = 600


  def solver: SmoothSolver = if(useCuda) new SmoothSolver_Cuda() else new SmoothSolver_EJML()
  override def postConfigure(log: NotebookOutput) = {
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    implicit val implicitLog = log
    if (Option(s3bucket).filter(!_.isEmpty).isDefined)
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
    log.onComplete(() => upload(log): Unit)
    var fullContent = ImageArtUtil.loadImageFile(contentUrl)
    //fullContent = Tensor.fromRGB(ImageUtil.resize(fullContent.toRgbImage, 1400, true))
    log.p(log.jpg(fullContent.toRgbImage, "Input Content"))

    val registration = registerWithIndexJPG(() => fullContent)
    try {

      val smallContent = Tensor.fromRGB(ImageUtil.resize(fullContent.toRgbImage, tile_size, true))
      log.out(log.jpg(smallContent.toRgbImage, "Small Image"))
      var recolored = recolor(smallContent)(smallContent.addRef())
      log.out(log.jpg(recolored.toRgbImage, "Recolored Small Image"))

      val priorTiles = List[String](
      ).map(ImageUtil.getTensor(_)).toBuffer

      for(res <- new GeometricSequence {
        override val min: Double = tile_size
        override val max: Double = fullContent.getDimensions()(0)
        override val steps = 2
      }.toStream.drop(1).map(_.toInt)) {

        val content = Tensor.fromRGB(ImageUtil.resize(fullContent.toRgbImage, res, true))
        val width = content.getDimensions()(0)
        val height = content.getDimensions()(1)
        val selectors_fade = TiledTrainable.selectors(tile_padding, width, height, tile_size, true)
        val selectors_sharp = TiledTrainable.selectors(tile_padding, width, height, tile_size, false)

        val enlarged = Tensor.fromRGB(ImageUtil.resize(recolored.toRgbImage, width, height))
        val tiles = for ((tileView_sharp, tileView_fade) <- selectors_sharp.zip(selectors_fade)) yield {
          if(priorTiles.isEmpty) withRetry() {
            var recoloredTile = tileView_sharp.eval(enlarged.addRef()).getData.get(0)
            val contentTile = tileView_sharp.eval(content.addRef()).getData.get(0)
            log.out(log.jpg(recoloredTile.toRgbImage, "Coarse Recoloring Tile"))
            log.out(log.jpg(contentTile.toRgbImage, "Content Tile"))
            recoloredTile = recolor(contentTile)(if(freshRecoloring) contentTile else recoloredTile)
            log.out(log.jpg(recoloredTile.toRgbImage, "Recolored Tile"))
            val maskTile = tileView_fade.eval(enlarged.map(x => 1)).getData.get(0)
            require(maskTile.getDimensions.toList == recoloredTile.getDimensions.toList)
            val product = recoloredTile.mapCoords(c => recoloredTile.get(c) * maskTile.get(c))
            log.out(log.jpg(product.toRgbImage, "Tile Product"))
            product
          } else {
            val prior = priorTiles.remove(0)
            log.out(log.jpg(prior.toRgbImage, "Tile Product"))
            prior
          }
        }
        recolored = TiledTrainable.reassemble(width, height, tiles, tile_padding, tile_size, true, false)
        //recolored = reassemble(enlarged, selectors_fade, tiles, log)
        log.out(log.jpg(recolored.toRgbImage, "Reassembled"))
      }

      null
    } finally {
      registration.foreach(_.stop()(s3client, ec2client))
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

  private def recolor(source: Tensor)(target: Tensor) = {
    val topology = new SearchRadiusTopology(source.addRef())
    topology.setSelfRef(true)
    topology.setVerbose(true)
    topology.setSpatialPriority(true)
    topology.setNeighborhoodSize(6)
    topology.setInitialRadius(2)
    val affinity = {
      val affinity = new RelativeAffinity(source, topology)
      affinity.setContrast(contrast)
      affinity.setGraphPower1(2)
      affinity.setMixing(mixing)
      affinity.wrap((graphEdges, innerResult) => RasterAffinity.adjust(graphEdges, innerResult, RasterAffinity.degree(innerResult), 0.5))
    }
    val operator = solver.solve(topology, affinity, lambda)
    val recoloredTensor = operator.apply(target)
    operator.freeRef()
    recoloredTensor
  }

}
