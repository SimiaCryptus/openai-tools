package com.simiacryptus.mindseye.lab

import org.apache.commons.io.FileUtils

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.meta._
import scala.meta.Term._
import scala.meta.inputs.Position

object TestOpenAIScaladoc {
  def main(args: Array[String]): Unit = {

    val root = new File("C:\\Users\\andre\\code\\all-projects\\deepartist\\deepartist.org\\")
    val language = "scala"

    println("Root: " + root.getAbsolutePath)
    val files = FileUtils.listFiles(root, Array(language), true).asScala.toList
    println(s"Found ${files.size} files")
    val version = 1
    for (file <- files.take(1)) {
      println("File: " + file.getAbsolutePath)
      val data = FileUtils.readFileToString(file, "UTF-8")

      import scala.meta._
      val tree: Source = data.parse[Source].get
      val edits = new ArrayBuffer[(scala.Int, String)]()
      tree.traverse({
        case _class : Defn.Class =>
          val definition : String = _class.transform({
            case _template: Template => _template.copy(stats = List.empty)
          }).toString()

          val start = _class.pos.start
          // TODO: Build Test to ensure comment has sufficient version metadata (e.g. @docgenVersion 1)
          // TODO: Build method to add version metadata to new comment
          // TODO:
          val currentVersion = getDocgenVersion(data, start)
          if(currentVersion.getOrElse(-1) < version) {
            println(s"Class Definition: $definition")
            edits ++= List((start, injectDocgenVersion(testComment(definition), version)))
          }
        case _method : Defn.Def if _method.paramss.size > 0 =>
          val definition : String = _method.transform({
            case _block: Block => _block.copy(stats = List.empty)
          }).toString()
          println(s"Method Definition: $definition")
          edits ++= List((_method.pos.start, injectDocgenVersion(testComment(definition), version)))
      })
      var workingData = data
      for((start, newComment) <- edits.sortBy(-_._1)) {
        val comment = preceedingComment(data, start)
        val oldCommentSize = comment.map(_.size).getOrElse(0)
        workingData = workingData.substring(0,start-oldCommentSize) + newComment + workingData.substring(start, workingData.length)
      }
      if(workingData != data) {
        FileUtils.write(file, workingData, "UTF-8")
      }

    }
  }

  def injectDocgenVersion(comment: String, version: Int): String = {
    val result = comment.replaceAll("\\s*/", "$1*\n$1*   @docgenVersion " + version + "\n$1*/")
    result
  }

  def getDocgenVersion(data: String, start: Int): Option[Int] = {
    val version = preceedingComment(data, start).flatMap(comment => {
      """@docgenVersion\s+(\d+)""".r.findFirstMatchIn(comment).map(_.group(1).toInt)
    })
    version
  }

  def testComment(str : String) = {
    s"""/*
       | * ${str.replaceAllLiterally("\n", "\n * ")}
       | */
       |""".stripMargin
  }

  def preceedingComment(data: String, start: Int) = {
    """(?s)^\s*/\*([^*]|\*(?!/))*\*/""".r.findFirstMatchIn(data.substring(0, start).reverse).map(_.group(0).reverse)
  }
}
