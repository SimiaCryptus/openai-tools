package com.simiacryptus.openai

import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{MapperFeature, ObjectMapper, SerializationFeature}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.apache.commons.io.FileUtils
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpRequestBase}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.util.EntityUtils

import java.io.{File, IOException}

trait OpenAIAPI {
  val apiBase = "https://api.openai.com/v1"
  //  val model = "text-ada-001"
  val model = "text-davinci-002"

  lazy val key = FileUtils.readFileToString(new File("openai.key"), "UTF-8").trim

  lazy val engines = getMapper().readValue(get(apiBase + "/engines"), classOf[Response]).data

  /** This scala function sends a POST request to the url with a json payload given by the map.
   * It returns a json string.
   *
   * @param url the url to send the request to
   * @param map the json payload to send
   * @return the json string returned by the request
   */
  def postRequest(url: String, map: Map[String, Any]): String = {
    post(url, getMapper.writeValueAsString(map))
  }

  def complete(completionRequest: CompletionRequest, model: String = model): TextCompletion = {
    val str = post(apiBase + "/engines/" + model + "/completions", getMapper.writeValueAsString(completionRequest))
    lazy val mapper = new ObjectMapper()
    //println(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(mapper.readTree(str)))
    getMapper().readValue(str, classOf[TextCompletion])
  }

  private def post(url: String, json: String, retries: Int = 3) : String = try {
    val client = HttpClientBuilder.create().build()
    val request = new HttpPost(url)
    request.addHeader("Content-Type", "application/json")
    request.addHeader("Accept", "application/json")
    authorize(request)
    request.setEntity(new StringEntity(json))
    val response = client.execute(request)
    val entity = response.getEntity()
    EntityUtils.toString(entity)
  } catch {
    case e : IOException if(retries>0) =>
      e.printStackTrace()
      Thread.sleep(15000)
      post(url, json, retries-1)
  }

  def getMapper() = {
    val mapper = new ObjectMapper()
    mapper
      .enable(SerializationFeature.INDENT_OUTPUT)
      .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
      .enable(MapperFeature.USE_STD_BEAN_NAMING)
      .registerModule(DefaultScalaModule)
      .activateDefaultTyping(mapper.getPolymorphicTypeValidator())
    mapper
  }

  def authorize(request: HttpRequestBase) = {
    request.addHeader("Authorization", "Bearer " + key)
  }

  def get(url: String): String = {
    val client = HttpClientBuilder.create().build()
    val request = new HttpGet(url)
    request.addHeader("Content-Type", "application/json")
    request.addHeader("Accept", "application/json")
    authorize(request)
    val response = client.execute(request)
    val entity = response.getEntity()
    EntityUtils.toString(entity)
  }

  def xmlFN
  (
    inputTag: String,
    outputTag: String,
    instruction: String,
    inputAttr: Map[String, String],
    outputAttr: Map[String, String]
  ) = (line: String) => {
    val inputAttributes = if (inputAttr.isEmpty) "" else (" " + inputAttr.map(t => s"""${t._1}="${t._2}""""))
    val outputAttributes = if (outputAttr.isEmpty) "" else (" " + outputAttr.map(t => s"""${t._1}="${t._2}""""))
    val request = CompletionRequest(
      prompt =
        s"""
           |<!-- $instruction -->
           |<$inputTag$inputAttributes>$line</$inputTag>
           |<$outputTag$outputAttributes>""".stripMargin.
          trim,
      temperature = 0.0,
      max_tokens = 250,
      stop = "</" + outputTag + ">"
    )
    try {
      val completion = complete(request)
      val completionOption = Option(completion.choices).flatMap(_.headOption).map(_.text.trim)
      if (completionOption.isEmpty) {
        line
      } else {
        val fixed = completionOption.get
        if (fixed.size >= 2 * line.size) {
          println(s"""REJECTED Fix Via OpenAI:\nFROM: $line\nTO:   $fixed""")
          line
        } else {
          println(s"""Fixed Via OpenAI:\nFROM: $line\nTO:   $fixed""")
          fixed
        }
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        line
    }
  }


}

case class Response
(
  `object`: String,
  data: Array[Engine]
)

case class Engine
(
  id: String,
  ready: Boolean,
  owner: String,
  `object`: String,
  created: Option[Int],
  permissions: Option[String],
  replicas: Option[Int],
  max_replicas: Option[Int]
)

case class TextCompletion
(
  id: String,
  `object`: String,
  created: Int,
  model: String,
  choices: Array[Choice],
  error: Option[ApiError]
)

case class ApiError
(
  message: String,
  `type`: String,
  param: String,
  code: Option[Double]
)

case class Choice
(
  text: String,
  index: Int,
  logprobs: Option[LogProbs],
  finish_reason: String
)

case class LogProbs
(
  tokens: Array[String],
  token_logprobs: Array[Double],
  top_logprobs: Array[ObjectNode],
  text_offset: Array[Int]
)

case class CompletionRequest
(
  prompt: String,
  temperature: Double = 0.0,
  max_tokens: Int = 256,
  stop: String = null,
  logprobs: Option[Int] = None,
  echo: Boolean = false
)
