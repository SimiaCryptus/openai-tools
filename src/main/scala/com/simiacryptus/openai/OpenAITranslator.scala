package com.simiacryptus.openai

import scala.collection.mutable

trait OpenAITranslator extends OpenAIAPI {

  val sourceLanguage: String
  val targetLanguage: String = "English"
  val targetDescription: String = "code comment"
  val startSequence = "/**\n *"
  val stopSequence = "*/"
  val continuePrompt = "\n * "

  private val cache = new mutable.HashMap[String, String]()

  def getDocumentationComment(codeData: String) = {
    cache.getOrElseUpdate(codeData, {
      var documentation = startSequence
      var prompt =
        s"""Translate this $sourceLanguage into an $targetLanguage $targetDescription:
           |
           |$sourceLanguage:
           |
           |$codeData
           |
           |$targetLanguage:
           |
           |$documentation""".stripMargin

      def advance() = {
        val response = complete(completionRequest = CompletionRequest(
          prompt = prompt,
          temperature = 0,
          max_tokens = 512,
          stop = stopSequence
        ), model = model)
        require(response.error.isEmpty, response.error.get)
        var next = response.choices.head.text
        if (response.choices.head.finish_reason == "stop") next = next + stopSequence
        documentation = documentation + next
        prompt = prompt + next
      }

      advance()
      while (!documentation.endsWith(stopSequence)) {
        //println(documentation)
        documentation = documentation.trim + continuePrompt
        prompt = prompt.trim + continuePrompt
        advance()
      }
      require(documentation.endsWith(stopSequence), "Completion did not contain result: " + documentation)

      documentation = reformat(documentation)
      println(documentation)
      documentation
    })

  }

  def reformat(documentation: String): String = {
    documentation.split('\n').map(_.trim).map(s => if (s.startsWith("*")) " " + s else s).mkString("\n")
  }

}
