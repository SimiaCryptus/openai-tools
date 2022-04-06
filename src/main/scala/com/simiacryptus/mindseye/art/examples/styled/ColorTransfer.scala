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
import com.simiacryptus.mindseye.art.util.ArtUtil._
import com.simiacryptus.mindseye.art.util.ColorTransforms._
import com.simiacryptus.mindseye.art.util.ImageArtUtil._
import com.simiacryptus.mindseye.art.util._
import com.simiacryptus.mindseye.lang.Tensor
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.JavaConverters.seqAsJavaListConverter

object ColorTransfer extends ColorTransfer with LocalRunner[Object] with NotebookRunner[Object]

class ColorTransfer extends ArtSetup[Object, ColorTransfer] with ArtworkStyleGalleries {

  val input = "file:///C:/Users/andre/Pictures/texture_sources/shutterstock_248374732_centered.jpg"
  val example = "file:///C:/Users/andre/Pictures/texture_sources/shutterstock_1060865300.jpg"

  override def inputTimeoutSeconds = 0

  val s3bucket: String = ""
  override def indexStr = "201"

  override def description = <div>
    Demonstrates an efficient color transfer algorithm
  </div>.toString.trim


  override def postConfigure(log: NotebookOutput) = {
    implicit val implicitLog = log
    implicit val s3client: AmazonS3 = S3Util.getS3(log.getArchiveHome)
    // First, basic configuration so we publish to our s3 site
    if (Option(s3bucket).filter(!_.isEmpty).isDefined)
      log.setArchiveHome(URI.create(s"s3://$s3bucket/$className/${log.getId}/"))
    log.onComplete(() => upload(log): Unit)

    val (colorReference, image) = log.subreport("Inputs", (sub: NotebookOutput) => {
      val colorReference = loadImages(sub, List(example).asJava, -1).map(img => {
        sub.p(sub.jpg(img, "Colorspace Reference"))
        Tensor.fromRGB(img)
      }).head
      val image = loadImages(sub, List(input).asJava, -1).map(img => {
        sub.p(sub.jpg(img, "Input Image"))
        Tensor.fromRGB(img)
      }).head
      (colorReference, image)
    })

    val iterations = 30
    val repeats = 3

    log.subreport("LAB", (sub: NotebookOutput) => {
      sub.p("Conversion Check")
      sub.p(sub.jpg(lab2rgb(rgb2lab(image.addRef())).toRgbImage, "LAB Test"))
      sub.p("Primary Margin Reshaping")
      val primary = colorTransferLAB(colorReference.addRef(), image.addRef())
      sub.p(sub.jpg(primary.toRgbImage, "LAB Remap"))
      sub.p("Primary+Random Margin Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferLAB(colorReference.addRef(), primary.addRef(), iterations).toRgbImage, "LAB Remap"))
      })
      sub.p("Random Marginal Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferLAB(colorReference.addRef(), image.addRef(), iterations).toRgbImage, "LAB Remap"))
      })
    })

    log.subreport("HSV", (sub: NotebookOutput) => {
      sub.p("Conversion Check")
      sub.p(sub.jpg(hsv2rgb(rgb2hsv(image.addRef())).toRgbImage, "HSV Test"))
      sub.p("Primary Margin Reshaping")
      val primary = colorTransferHSV(colorReference.addRef(), image.addRef())
      sub.p(sub.jpg(primary.toRgbImage, "HSV Remap"))
      sub.p("Primary+Random Margin Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferHSV(colorReference.addRef(), primary.addRef(), iterations).toRgbImage, "HSV Remap"))
      })
      sub.p("Random Marginal Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferHSV(colorReference.addRef(), image.addRef(), iterations).toRgbImage, "HSV Remap"))
      })
    })

    log.subreport("HSL", (sub: NotebookOutput) => {
      sub.p("Conversion Check")
      sub.p(sub.jpg(hsl2rgb(rgb2hsl(image.addRef())).toRgbImage, "HSL Test"))
      sub.p("Primary Margin Reshaping")
      val primary = colorTransferHSL(colorReference.addRef(), image.addRef())
      sub.p(sub.jpg(primary.toRgbImage, "HSL Remap"))
      sub.p("Primary+Random Margin Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferHSL(colorReference.addRef(), primary.addRef(), iterations).toRgbImage, "HSL Remap"))
      })
      sub.p("Random Marginal Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferHSL(colorReference.addRef(), image.addRef(), iterations).toRgbImage, "HSL Remap"))
      })
    })

    log.subreport("XYZ", (sub: NotebookOutput) => {
      sub.p("Conversion Check")
      sub.p(sub.jpg(xyz2rgb(rgb2xyz(image.addRef())).toRgbImage, "XYZ Test"))
      sub.p("Primary Margin Reshaping")
      val primary = colorTransferXYZ(colorReference.addRef(), image.addRef())
      sub.p(sub.jpg(primary.toRgbImage, "XYZ Remap"))
      sub.p("Primary+Random Margin Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferXYZ(colorReference.addRef(), primary.addRef(), iterations).toRgbImage, "XYZ Remap"))
      })
      sub.p("Random Marginal Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(colorTransferXYZ(colorReference.addRef(), image.addRef(), iterations).toRgbImage, "XYZ Remap"))
      })
    })

    log.subreport("RGB", (sub: NotebookOutput) => {
      sub.p("Primary Margin Reshaping")
      val primary = ColorTransforms.colorTransfer_primaries(colorReference.addRef(), image.addRef())
      sub.p(sub.jpg(primary.toRgbImage, "RGB Remap"))
      sub.p("Primary+Random Margin Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(ColorTransforms.colorTransfer_random(colorReference.addRef(), primary.addRef(), iterations).toRgbImage, "RGB Remap"))
      })
      sub.p("Random Marginal Reshaping")
      (1 to repeats).foreach(_=>{
        sub.p(sub.jpg(ColorTransforms.colorTransfer_random(colorReference.addRef(), image.addRef(), iterations).toRgbImage, "RGB Remap"))
      })
    })

    null
  }

}


