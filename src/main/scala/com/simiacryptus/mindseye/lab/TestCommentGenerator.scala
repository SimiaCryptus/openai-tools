package com.simiacryptus.mindseye.lab

trait TestCommentGenerator extends CommentGenerator {

  def testComment(codeData: String, indent: String): String = {
    s"""$indent/*
       |$indent * ${codeData.replaceAllLiterally("\n", s"\n$indent * ")}
       |$indent */
       |$indent""".stripMargin
  }

}
