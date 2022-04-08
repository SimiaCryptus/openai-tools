package com.simiacryptus.mindseye.lab

import com.fasterxml.jackson.databind.{MapperFeature, ObjectMapper, SerializationFeature}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.simiacryptus.mindseye.lab.GenerateScaladoc.apiBase
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpRequestBase}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.util.EntityUtils

trait OpenAIAPI {
  val apiBase = "https://api.openai.com/v1"
  //  val model = "text-ada-001"
  val model = "text-davinci-002"
  val key = "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"

  lazy val engines = getMapper().readValue(getRequest(apiBase + "/engines"), classOf[Response]).data

  /** This scala function sends a POST request to the url with a json payload given by the map.
   * It returns a json string.
   *
   * @param url the url to send the request to
   * @param map the json payload to send
   * @return the json string returned by the request
   */
  def postRequest(url: String, map: Map[String, Any]): String = {
    val json = getMapper.writeValueAsString(map)
    val client = HttpClientBuilder.create().build()
    val request = new HttpPost(url)
    request.addHeader("Content-Type", "application/json")
    request.addHeader("Accept", "application/json")
    authorize(request)
    request.setEntity(new StringEntity(json))
    val response = client.execute(request)
    val entity = response.getEntity()
    EntityUtils.toString(entity)
  }

  def complete(model: String, completionRequest: CompletionRequest): String = {
    val json = getMapper.writeValueAsString(completionRequest)
    val client = HttpClientBuilder.create().build()
    val request = new HttpPost(apiBase + "/engines/" + model + "/completions")
    request.addHeader("Content-Type", "application/json")
    request.addHeader("Accept", "application/json")
    authorize(request)
    request.setEntity(new StringEntity(json))
    val response = client.execute(request)
    val entity = response.getEntity()
    EntityUtils.toString(entity)
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

  def getRequest(url: String): String = {
    val client = HttpClientBuilder.create().build()
    val request = new HttpGet(url)
    request.addHeader("Content-Type", "application/json")
    request.addHeader("Accept", "application/json")
    authorize(request)
    val response = client.execute(request)
    val entity = response.getEntity()
    EntityUtils.toString(entity)
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
  logprobs: Option[Double],
  finish_reason: String
)

case class CompletionRequest
(
  prompt: String,
  temperature: Double,
  max_tokens: Int,
  stop: String
)
