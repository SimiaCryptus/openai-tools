package com.simiacryptus.mindseye.lab

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration}
import com.github.javaparser.ast.visitor.VoidVisitorAdapter
import org.apache.commons.io.FileUtils

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object GenerateJavadoc extends OpenAICommentGenerator {
  val root = new File("""C:\Users\andre\code\all-projects\""")
  val language = "java"
  val version_class = 9
  val version_method = 9

  def main(args: Array[String]): Unit = {


    println("Root: " + root.getAbsolutePath)
    val files = FileUtils.listFiles(root, Array(language), true).asScala.toList
    println(s"Found ${files.size} files")
    for (file <- files) {
      println("File: " + file.getAbsolutePath)
      val data = FileUtils.readFileToString(file, "UTF-8")

      val compilationUnit: CompilationUnit = StaticJavaParser.parse(data)

      val edits = new ArrayBuffer[(scala.Int, String)]()
      compilationUnit.accept(new VoidVisitorAdapter[Void] {
        override def visit(n: ClassOrInterfaceDeclaration, arg: Void): Unit = {
          //          val definition : String = _class.transform({
          //            case _template: Template => _template.copy(stats = List.empty)
          //          }).toString()
          //
          //          val start = _class.pos.start
          //          if (getDocgenVersion(data, start).getOrElse(-1) < version_class) {
          //            println(s"Class Definition: $definition")
          //            val comment = testComment(definition, spaces(_class.pos.startColumn))
          //            edits ++= List((start, injectDocgenVersion(comment, version_class)))
          //          }
          //      tree.traverse({
          super.visit(n, arg)
        }

        override def visit(n: MethodDeclaration, arg: Void): Unit = {
          //          val definition : String = _method.transform({
          //            case _block: Block => _block.copy(stats = List.empty)
          //          }).toString()
          //          val start = _method.pos.start
          //          if (getDocgenVersion(data, start).getOrElse(-1) < version_method) {
          //            println(s"Method Definition: $definition")
          //            val info = if(definition.length < 64 && _method.toString().length < 512) {
          //              _method.toString()
          //            } else {
          //              definition
          //            }
          //            val comment = testComment(info, spaces(_method.pos.startColumn))
          //            edits ++= List((start, injectDocgenVersion(comment, version_method)))
          //          }
          //
          super.visit(n, arg)
        }
      }, null)

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

  def spaces(column: Int): String = {
    String.copyValueOf(Stream.continually(' ').take(column).toArray)
  }

  def injectDocgenVersion(comment: String, version: Int): String = {
    comment.replaceAll("""([\t ]*)\*/""", "$1*\n$1*   @docgenVersion " + version + "\n$1*/")
  }

  def getDocgenVersion(data: String, start: Int): Option[Int] = {
    preceedingComment(data, start).flatMap(comment => {
      """@docgenVersion\s+(\d+)""".r.findFirstMatchIn(comment).map(_.group(1).toInt)
    })
  }

  def preceedingComment(data: String, start: Int) = {
    """(?s)^\s*/\*([^*]|\*(?!/))*\*/[ \t]*""".r.findFirstMatchIn(data.substring(0, start).reverse).map(_.group(0).reverse)
  }


}


