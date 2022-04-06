package com.simiacryptus.mindseye.art.examples.zoomrotor

import com.simiacryptus.mindseye.art.util.RotorArt

trait ArtSource[U <: ArtSource[U]] extends RotorArt[U] {

  def border: Double

  def magnification: Array[Double]

  def rotationalSegments: Int

  def rotationalChannelPermutation: Array[Int]

  def styles: Array[String]

  def keyframes: Array[String]

}
