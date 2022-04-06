package com.simiacryptus.mindseye.art.examples.rotor

import com.simiacryptus.aws.exe.EC2NodeSettings
import com.simiacryptus.mindseye.lang.cudnn.{CudaMemory, Precision}
import com.simiacryptus.sparkbook.{AWSNotebookRunner, EC2Runner}

object AnimatedRotorEC2 extends AnimatedRotor with EC2Runner[Object] with AWSNotebookRunner[Object] {
  override val s3bucket: String = "test.deepartist.org"

  override def nodeSettings: EC2NodeSettings = EC2NodeSettings.P3_2XL

  override def maxHeap: Option[String] = Option("50g")

  override def className: String = "AnimatedRotor"

  override def javaProperties: Map[String, String] = super.javaProperties ++ Map(
    "MAX_TOTAL_MEMORY" -> (10 * CudaMemory.GiB).toString,
    "MAX_DEVICE_MEMORY" -> (10 * CudaMemory.GiB).toString,
    "CUDA_DEFAULT_PRECISION" -> Precision.Float.name,
    "MAX_FILTER_ELEMENTS" -> (256 * CudaMemory.MiB).toString,
    "MAX_IO_ELEMENTS" -> (256 * CudaMemory.MiB).toString
  )
}
