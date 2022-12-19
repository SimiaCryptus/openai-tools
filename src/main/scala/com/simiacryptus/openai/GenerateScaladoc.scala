package com.simiacryptus.openai

import org.apache.commons.io.FileUtils

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.iterableAsScalaIterableConverter
import scala.meta.Term.Block

object GenerateScaladoc extends OpenAICommentGenerator {
  val root = new File("""C:\Users\andre\code\all-projects\deepartist\deepartist.org\""")
  val languageExtension = "scala"
  val version_class = 9
  val version_method = 9

  override val sourceLanguage = "Scala"
  override val targetLanguage = "English"
  override val targetDescription = "description in ScalaDoc format"

  def main(args: Array[String]): Unit = {


    println("Root: " + root.getAbsolutePath)
    val files = FileUtils.listFiles(root, Array(languageExtension), true).asScala.toList
    println(s"Found ${files.size} files")
    for (file <- files) {
      println("File: " + file.getAbsolutePath)
      val data = FileUtils.readFileToString(file, "UTF-8")

      import scala.meta._
      val tree: Source = data.parse[Source].get
      val edits = new ArrayBuffer[(scala.Int, String)]()
      tree.traverse({

        case _class: Defn.Class =>
          val definition: String = _class.transform({
            case _template: Template => _template.copy(stats = List.empty)
          }).toString()

          val start = _class.pos.start
          if (GenerateJavadoc.getDocgenVersion(data, start).getOrElse(-1) < version_class) {
            println(s"Class Definition: $definition")
            val comment = getDocumentationComment(definition, GenerateJavadoc.spaces(_class.pos.startColumn))
            edits ++= List((start, GenerateJavadoc.injectDocgenVersion(comment, version_class)))
          }

        case _method: Defn.Def if _method.paramss.size > 0 =>
          val definition: String = _method.transform({
            case _block: Block => _block.copy(stats = List.empty)
          }).toString()
          val start = _method.pos.start
          if (GenerateJavadoc.getDocgenVersion(data, start).getOrElse(-1) < version_method) {
            println(s"Method Definition: $definition")
            val info = if (definition.length < 64 && _method.toString().length < 512) {
              _method.toString()
            } else {
              definition
            }
            val comment = getDocumentationComment(info, GenerateJavadoc.spaces(_method.pos.startColumn))
            edits ++= List((start, GenerateJavadoc.injectDocgenVersion(comment, version_method)))
          }

      })
      var workingData = data
      for ((start, newComment) <- edits.sortBy(-_._1)) {
        val comment = GenerateJavadoc.preceedingComment(data, start)
        val oldCommentSize = comment.map(_.size).getOrElse(0)
        workingData = workingData.substring(0, start - oldCommentSize) + newComment + workingData.substring(start, workingData.length)
      }
      if (workingData != data) {
        FileUtils.write(file, workingData, "UTF-8")
      }

    }
  }


}
