package com.simiacryptus.openai

trait OpenAICommentGenerator extends CommentGenerator with OpenAITranslator {

  def getDocumentationComment(codeData: String, indent: String): String = {
    indent + getDocumentationComment(codeData)
      .replaceAllLiterally("\n", "\n" + indent)
      .replaceAll("\n{2,}", "\n").trim + "\n" + indent
  }
}
