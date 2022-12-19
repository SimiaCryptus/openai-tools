package com.simiacryptus.openai

trait CommentGenerator {
  def getDocumentationComment(codeData: String, indent: String): String
}
