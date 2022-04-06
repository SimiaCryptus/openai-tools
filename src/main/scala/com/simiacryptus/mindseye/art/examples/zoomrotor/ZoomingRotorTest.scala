package com.simiacryptus.mindseye.art.examples.zoomrotor

import com.simiacryptus.mindseye.art.models.VGG19
import com.simiacryptus.mindseye.art.ops.{ContentMatcher, GramMatrixEnhancer, MomentMatcher}
import com.simiacryptus.mindseye.art.util.{ImageOptimizer, VisualNetwork, VisualStyleNetwork}
import com.simiacryptus.mindseye.lang.{Layer, Tensor}
import com.simiacryptus.mindseye.opt.region.TrustRegion
import com.simiacryptus.notebook.NotebookOutput

class ZoomingRotorTest extends ZoomingRotor[ZoomingRotorTest] with CosmicArt[ZoomingRotorTest] {
  override val s3bucket: String = null
  override val resolution = 320
  override val totalZoom = 0.25
  override val stepZoom = 1.0
  override val border: Double = 0.125
  override val innerCoeff = 0
  override val enhancementCoeff: Double = 0

  override def inputTimeoutSeconds: Int = 0

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
      var style: VisualNetwork = new VisualStyleNetwork(
        styleLayers = List(
          VGG19.VGG19_1b2,
          VGG19.VGG19_1c1,
          VGG19.VGG19_1c2,
          VGG19.VGG19_1c3,
          VGG19.VGG19_1c4
        ),
        styleModifiers = List(
          new MomentMatcher().scale(Math.pow(2, -enhancementCoeff * 2))
        ).map(_.withMask(outerMask.addRef())),
        styleUrls = styles,
        magnification = magnification,
        viewLayer = dims => getKaleidoscope(dims.toArray)
      ) + new VisualStyleNetwork(
        styleLayers = List(
          VGG19.VGG19_1d1,
          VGG19.VGG19_1d2,
          VGG19.VGG19_1d3,
          VGG19.VGG19_1d4
        ),
        styleModifiers = List(
          new GramMatrixEnhancer().setMinMax(-5, 5).scale(Math.pow(2, enhancementCoeff * 2))
        ).map(_.withMask(outerMask.addRef())),
        styleUrls = styles,
        magnification = magnification,
        viewLayer = dims => getKaleidoscope(dims.toArray)
      )
      if (innerCoeff > 0) style = style.asInstanceOf[VisualStyleNetwork].withContent(
        contentLayers = List(
          VGG19.VGG19_0a
        ), contentModifiers = List(
          new ContentMatcher()
            .withMask(innerMask.addRef())
            .scale(innerCoeff)
        ))
      style
    })
  }

}
