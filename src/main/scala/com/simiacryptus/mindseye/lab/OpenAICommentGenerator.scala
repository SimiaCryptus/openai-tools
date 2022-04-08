package com.simiacryptus.mindseye.lab

trait OpenAICommentGenerator extends CommentGenerator with OpenAIAPI {

  val sourceLanguage = "Scala"
  val targetLanguage = "English"
  val targetDescription = "code comment"

  def getDocumentationComment(codeData: String, indent: String): String = {
    var documentation = "/**\n *"
    var prompt = "Translate this " + sourceLanguage + " into an " + targetLanguage + " " + targetDescription + ":\n\n" + sourceLanguage + ":\n\n" + codeData + "\n\n" + targetLanguage + ":\n\n" + documentation

    def advance() = {
      val response = getMapper().readValue(complete(
        model = model,
        completionRequest = CompletionRequest(
          prompt = prompt,
          temperature = 0,
          max_tokens = 512,
          stop = "*/"
        )), classOf[TextCompletion])
      require(response.error.isEmpty, response.error.get)
      var next = response.choices.head.text
        .replaceAllLiterally("\n", "\n" + indent)
        .replaceAll("\n{2,}", "\n")
      if (response.choices.head.finish_reason == "stop") next = next + "*/"
      documentation = documentation + next
      prompt = prompt + next
    }

    advance()
    while (!documentation.endsWith("*/")) {
      //println(documentation)
      val prod = "\n * "
      documentation = documentation.trim + prod
      prompt = prompt.trim + prod
      advance()
    }

    require(documentation.endsWith("*/"), "Completion did not contain valid comment: " + documentation)
    documentation = documentation.split('\n').map(_.trim).map(s => if (s.startsWith("*")) " " + s else s).mkString("\n")
    println(documentation)
    indent + documentation + "\n" + indent
  }

}
