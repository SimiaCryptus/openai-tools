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

package com.simiacryptus.mindseye.art.libs

import com.simiacryptus.mindseye.art.examples.rotor.TextureTiledRotor
import com.simiacryptus.mindseye.art.examples.styled.MultiStylized
import com.simiacryptus.mindseye.art.examples.texture.{BigTexture, MultiTexture}
import com.simiacryptus.mindseye.art.examples.zoomrotor.ZoomingRotor
import com.simiacryptus.sparkbook.aws.{P2_XL, P3_2XL}
import com.simiacryptus.sparkbook.util.LocalRunner
import com.simiacryptus.sparkbook.{InteractiveSetup, NotebookRunner}

object StandardLibraryNotebook extends StandardLibraryNotebook with LocalRunner[Object] with NotebookRunner[Object] {
  override def install: Boolean = false

  override val s3bucket: String = "test.deepartist.org"
}

class StandardLibraryNotebook extends LibraryNotebook {

  def applications: Map[String, List[ApplicationInfo[_ <: InteractiveSetup[_, _]]]] = Map(
    "Textures" -> List(
      ApplicationInfo(
        "MultiTexture", Map(
          "default" -> (() => new MultiTexture_default())
        )),
      ApplicationInfo(
        "BigTexture", Map(
          "default" -> (() => new BigTexture_default())
        )),
      ApplicationInfo(
        "TextureTiledRotor", Map(
          "default" -> (() => new TextureTiledRotor_default())
        )),
      ApplicationInfo(
        "ZoomingRotor", Map(
          "default" -> (() => new ZoomingRotor_default())
        ))
    ),
    "Stylized" -> List(
      ApplicationInfo(
        "MultiStylized", Map(
          "default" -> (() => new MultiStylized_default())
        ))
    )
  )

}

class MultiTexture_default extends MultiTexture {
  override def className: String = "MultiTexture"

  override val s3bucket: String = StandardLibraryNotebook.s3bucket
  override val styleUrls = Array("upload:Style")

  override def inputTimeoutSeconds: Int = Integer .MAX_VALUE

  override val initUrls = Array(
    "50 + noise * 0.5",
    "plasma",
    "50 + noise * 0.5",
    "plasma"
  )
}

class MultiStylized_default extends MultiStylized {
  override def className: String = "MultiStylized"

  override val s3bucket: String = StandardLibraryNotebook.s3bucket
  override val styleUrls = Array("upload:Style")
  override val contentUrl: String = "upload:Content"

  override def inputTimeoutSeconds: Int = Integer.MAX_VALUE
}

class BigTexture_default extends BigTexture {
  override def className: String = "BigTexture"
  override val s3bucket: String = StandardLibraryNotebook.s3bucket
  override def inputTimeoutSeconds: Int = Integer.MAX_VALUE

  override val styleUrl: String = "upload:Style"
  override val initUrl: String = "plasma"
  override val aspectRatio: Double = 0.5774
  override val minRes: Int = 200
}

class TextureTiledRotor_default extends TextureTiledRotor {
  override def className: String = "TextureTiledRotor"
  override val s3bucket: String = StandardLibraryNotebook.s3bucket
  override val reportingBucket: String = StandardLibraryNotebook.s3bucket
  override def inputTimeoutSeconds: Int = Integer.MAX_VALUE

  override val styleUrl: String = "upload:Style"
  override val aspectRatio: Double = 1/0.5774
  override val rotationalSegments: Int = 6
  override val rotationalChannelPermutation: Array[Int] = Array(1,2,3)
  override val initUrls: Array[String] = Array(
    "50 + noise * 0.5",
    "plasma",
    "50 + noise * 0.5",
    "plasma"
  )
}

class ZoomingRotor_default extends ZoomingRotor {
  override def className: String = "ZoomingRotor"
  override val s3bucket: String = StandardLibraryNotebook.s3bucket
  override def inputTimeoutSeconds: Int = Integer.MAX_VALUE
  override val reportingBucket: String = StandardLibraryNotebook.s3bucket

  override val rotationalSegments: Int = 6
  override val rotationalChannelPermutation: Array[Int] = Array(1,2,3)
  override val keyframes: Array[String] = Array.empty
}
