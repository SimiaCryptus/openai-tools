package com.simiacryptus.mindseye.art.examples.zoomrotor

trait CosmicArt[U <: CosmicArt[U]] extends ArtSource[U] {
  override val border: Double = 0.15
  override val magnification: Array[Double] = Array(4.0)
  override val rotationalSegments = 1
  override val rotationalChannelPermutation: Array[Int] = Array(1, 2, 3)
  override val styles: Array[String] = Array(
    "upload:Style"
  )
  override val keyframes = Array(
    "http://simiacryptus.s3.us-west-2.amazonaws.com/photos/cosmic_zoom/galaxy.jpg",
    "http://simiacryptus.s3.us-west-2.amazonaws.com/photos/cosmic_zoom/earth.jpg",
    "http://simiacryptus.s3.us-west-2.amazonaws.com/photos/cosmic_zoom/tree.jpg",
    "http://simiacryptus.s3.us-west-2.amazonaws.com/photos/cosmic_zoom/bug.jpg",
    "http://simiacryptus.s3.us-west-2.amazonaws.com/photos/cosmic_zoom/germs.jpg"
  )
}
