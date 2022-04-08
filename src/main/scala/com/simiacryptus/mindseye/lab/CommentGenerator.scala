package com.simiacryptus.mindseye.lab


trait CommentGenerator {
  def getDocumentationComment(codeData : String, indent : String): String
}