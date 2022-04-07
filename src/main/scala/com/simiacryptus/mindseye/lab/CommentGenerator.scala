package com.simiacryptus.mindseye.lab


trait CommentGenerator {
  def testComment(codeData : String, indent : String): String
}