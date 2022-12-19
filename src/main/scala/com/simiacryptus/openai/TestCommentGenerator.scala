package com.simiacryptus.openai

trait TestCommentGenerator extends CommentGenerator {

  def getDocumentationComment(codeData: String, indent: String): String = {
    s"""$indent/*
       |$indent * ${codeData.replaceAllLiterally("\n", s"\n$indent * ")}
       |$indent */
       |$indent""".stripMargin
  }

}
