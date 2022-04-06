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
import java.util

import com.amazonaws.services.s3.AmazonS3
import com.google.gson.{JsonElement, JsonObject}
import com.simiacryptus.aws.S3Util
import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops._
import com.simiacryptus.mindseye.art.util.ArtSetup.ec2client
import com.simiacryptus.mindseye.art.util.ImageArtUtil._
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, _}
import com.simiacryptus.mindseye.lang.{DataSerializer, Layer, LayerBase, Result, Tensor, TensorArray}
import com.simiacryptus.mindseye.layers.WrapperLayer
import com.simiacryptus.mindseye.layers.cudnn.conv.{ConvolutionLayer, FullyConnectedLayer}
import com.simiacryptus.mindseye.layers.cudnn.{ActivationLayer, ImgBandBiasLayer, ImgConcatLayer, ProductLayer, SquareActivationLayer, StochasticSamplingSubnetLayer, SumInputsLayer, ValueLayer}
import com.simiacryptus.mindseye.layers.java.{CoordinateAssemblyLayer, CoordinateDisassemblyLayer, DropoutNoiseLayer}
import com.simiacryptus.mindseye.network.PipelineNetwork
import com.simiacryptus.mindseye.opt.TrainingResult
import com.simiacryptus.mindseye.opt.region.TrustRegion
import com.simiacryptus.mindseye.util.ImageUtil
import com.simiacryptus.notebook.NotebookOutput
import com.simiacryptus.ref.wrappers.{RefArrayList, RefList}
import com.simiacryptus.sparkbook.NotebookRunner
import com.simiacryptus.sparkbook.util.LocalRunner

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.immutable

object NeuralPainting extends NeuralPainting with LocalRunner[Object] with NotebookRunner[Object]

  class NeuralPainting extends ArtSetup[Object, NeuralPainting] with ArtworkStyleGalleries {

  val contentUrl = "file:///C:/Users/andre/Pictures/Artistry/chimps/son_at_work.jpg"

  val resolutions = GeometricSequenceJson(min = 16, max = 512, steps = 4)
  override def inputTimeoutSeconds = 0
//    override def inputTimeoutSeconds = 3600

  //val s3bucket: String = "test.deepartist.org"
  val s3bucket: String = ""
  val models = 16

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

    val contentImage = log.subreport("Content", (sub: NotebookOutput) => {
      val contentImage = loadImages(sub, List(contentUrl).asJava, -1).head
      sub.p(sub.jpg(contentImage, "Input Style"))
      contentImage
    })

    val paintingNetworks = (1 to models).map(i => {
      val paintingNetwork = new PipelineNetwork
      val disassemblyLayer = new CoordinateDisassemblyLayer(false)
      disassemblyLayer.setMinX(-1)
      disassemblyLayer.setMinY(-1)
      val coordNode = paintingNetwork.add(disassemblyLayer)
      val squareNode = paintingNetwork.add(new SquareActivationLayer())
      //squareNode.freeRef()
      paintingNetwork.add(new ProductLayer(), coordNode, squareNode.addRef()).freeRef
      paintingNetwork.add(new ImgConcatLayer(), coordNode, squareNode, paintingNetwork.getHead).freeRef
      val hiddenSize = 1000
      val inputSize = 6
      val outputSize = 3
      paintingNetwork.add(new ImgBandBiasLayer(inputSize)).freeRef()
      paintingNetwork.add({
              val layer = new ConvolutionLayer(1, 1, inputSize, hiddenSize)
              val kernel = layer.getKernel
        //        kernel.setByCoord(c=>{
        //          kernel.get(c.getIndex) * 1e-2
        //        })
              kernel.freeRef()
              layer
            }.explode()).freeRef
      //paintingNetwork.add(new ImgBandBiasLayer(hiddenSize)).freeRef()
      paintingNetwork.add(new DropoutNoiseLayer()).freeRef
      paintingNetwork.add(new ActivationLayer(ActivationLayer.Mode.RELU)).freeRef
      paintingNetwork.add({
              val layer = new ConvolutionLayer(1, 1, hiddenSize, outputSize)
              val kernel = layer.getKernel
        //        kernel.setByCoord(c=>{
        //          kernel.get(c.getIndex) * 1e-2
        //        })
              kernel.freeRef()
              layer
            }.explode()).freeRef
      paintingNetwork.add(new ImgBandBiasLayer(3)).freeRef
      paintingNetwork.add(new CoordinateAssemblyLayer(false),
        paintingNetwork.getHead,
        paintingNetwork.getInput(0)).freeRef
      paintingNetwork.setName(s"Painter $i")
      new StochasticSamplingSubnetLayer(paintingNetwork,3)
    })

//    var details = true
    var details = false
    resolutions.toStream.map(_.round.toInt).foreach(width => {
      val height = (width.toDouble * contentImage.getHeight.toDouble / contentImage.getWidth.toDouble).floor.toInt
      log.subreport(s"Resolution: $width x $height", (sub: NotebookOutput) => {
        res(contentImage, paintingNetworks, width, height, details)(sub, s3client)
        details = false
      }: Unit)
    })
    null
  }

  def res(contentImage: BufferedImage, paintingNetworks: immutable.IndexedSeq[WrapperLayer], width: Int, height: Int, details: Boolean)(implicit log: NotebookOutput, s3client: AmazonS3) = {
    val canvas = new Tensor(width, height, 3)
    val antiFreezeLayer = new AntiFreezeLayer(new PipelineNetwork(1))
    val trainable = stylePrepFn(
      contentTensor = Tensor.fromRGB(ImageUtil.resize(contentImage, width, height)),
      network = new VisualStyleContentNetwork(
        viewLayer = _ => List(antiFreezeLayer),
        contentLayers = List(VGG19.VGG19_0b),
        contentModifiers = List(new ContentMatcher())
      ),
      canvas = canvas.addRef()
    )
    for (paintingNetwork <- paintingNetworks) {
      antiFreezeLayer.setInner(paintingNetwork.getInner)
      val registration = registerWithIndexJPG(() => antiFreezeLayer.simpleEval(canvas.addRef()))
      try {
        val optimizer = new ImageOptimizer {
          override val trainingMinutes = 120
          override val trainingIterations = 50
          override val maxRate = 1e9
          override val maxRetries = 0
          override def renderingNetwork(dims: Seq[Int]): Layer = antiFreezeLayer.addRef()
          override def trustRegion(layer: Layer): TrustRegion = {
            if(null != layer) layer.freeRef()
            null
          }
        }
        if(details) {
          NetworkUtil.graph(paintingNetwork.getInner.asInstanceOf[PipelineNetwork], new TensorArray(new Tensor(width, height, 3)))
        }
        val result = optimizer.optimize(canvas.addRef(), trainable.addRef())
        if(details && result.terminationCause == TrainingResult.TerminationCause.Failed) {
          NetworkUtil.graph(paintingNetwork.getInner.asInstanceOf[PipelineNetwork], new TensorArray(canvas.addRef()))
        }
      } finally {
        registration.foreach(_.stop()(s3client, ec2client))
      }
    }
    antiFreezeLayer.freeRef()
    canvas.freeRef()
  }
}


class AntiFreezeLayer(var layer: Layer) extends LayerBase {

  def setInner(network: Layer): Unit = {
    layer.freeRef()
    layer = network
  }

  override def isFrozen: Boolean = {
    //super.isFrozen
    false
  }

  override def setFrozen(frozen: Boolean): Unit = {
    //super.setFrozen(frozen)
    println("Ignoring setFrozen")
  }

  override def eval(array: Result*): Result = layer.eval(array: _*)

  override def state(): RefList[Array[Double]] = new RefArrayList[Array[Double]]()

  override def getJson(resources: util.Map[CharSequence, Array[Byte]], dataSerializer: DataSerializer): JsonElement = new JsonObject
}

