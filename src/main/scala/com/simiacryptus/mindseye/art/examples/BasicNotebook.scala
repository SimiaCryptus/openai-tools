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

import java.awt.{Font, Graphics2D}
import java.net.URI

import com.amazonaws.services.s3.AmazonS3
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.util._
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.sparkbook._
import com.simiacryptus.sparkbook.util.Java8Util._
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.JavaConverters.seqAsJavaListConverter

object BasicNotebook extends BasicNotebook with LocalRunner[Object] with NotebookRunner[Object] {
  override def http_port: Int = 1081
}

class BasicNotebook extends ArtSetup[Object, BasicNotebook] with ArtworkStyleGalleries {

  val styleUrls = Array(
    //"upload:Image"
    "cubism_portraits"
  )
  val s3bucket: String = "test.deepartist.org"
  val message = ""
  val resolution = -1

  override def indexStr = "000"

  override def description = <div>
    A very basic notebook that displays images with a simple edit.
    No AI code, just a demo of the publishing system used.
  </div>.toString.trim

  override def inputTimeoutSeconds = 3600

  override def postConfigure(log: NotebookOutput) = {
    implicit val l = log
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    // First, basic configuration so we publish to our s3 site
    if (Option(s3bucket).filter(!_.isEmpty).isDefined)
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
    log.onComplete(() => upload(log): Unit)
    // Now we evaluate the drawing code inside a logged eval block.
    // This will publish the code, the result, any logs, the duration, and also link to github.
    val canvas = (for (canvas <- ImageArtUtil.loadImages(log, styleGalleries_lowRes(styleUrls).asJava, resolution.toInt)) yield {
      log.eval(() => {
        if(message.nonEmpty) {
          val graphics = canvas.getGraphics.asInstanceOf[Graphics2D]
          graphics.setFont(new Font("Calibri", Font.BOLD, 42))
          graphics.drawString(message, 10, 50)
        }
        canvas
      })
    }).head
    // Usually not on one line, this code publishes our result to the site's index so it is linked from the homepage.
    registerWithIndexJPG(() => Tensor.fromRGB(canvas)).foreach(_.stop()(s3client, ArtSetup.ec2client))
    null
  }
}
