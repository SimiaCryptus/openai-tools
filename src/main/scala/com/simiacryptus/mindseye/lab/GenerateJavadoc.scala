package com.simiacryptus.mindseye.lab

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.visitor.VoidVisitorAdapter
import org.apache.commons.io.FileUtils

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random.shuffle

object GenerateJavadoc extends OpenAICommentGenerator {
  val root = new File("""C:\Users\andre\code\all-projects\mindseye\mindseye-java\""")
  val languageExtension = "java"
  val version_class = 9
  val version_method = 9
  val maxFiles = 1000
  val maxMethodBodySize = 256

  override val sourceLanguage = "Java"
  override val targetLanguage = "English"
  override val targetDescription = "description in JavaDoc format"

  def main(args: Array[String]): Unit = {

    println("Root: " + root.getAbsolutePath)
    val files = FileUtils.listFiles(root, Array(languageExtension), true).asScala.toList
    println(s"Found ${files.size} files")
    for (file <- shuffle(files).take(maxFiles)) {
      println("File: " + file.getAbsolutePath)
      val data = FileUtils.readFileToString(file, "UTF-8")

      val compilationUnit: CompilationUnit = StaticJavaParser.parse(data)

      val edits = new ArrayBuffer[(scala.Int, String)]()
      compilationUnit.accept(new VoidVisitorAdapter[Void] {
        override def visit(n: ClassOrInterfaceDeclaration, arg: Void): Unit = {

          val declaration = n.clone()
          val childNodes = declaration.getChildNodes
          declaration.setJavadocComment("")
          declaration.setComment(null)
          declaration.getChildNodes.subList(0,childNodes.size()).asScala.toArray.foreach({
            case _ : FieldDeclaration => // Keep
            case node => declaration.remove(node)
          })
          val begin = n.getRange.get().begin
          val end = n.getRange.get().end
          val start = data.split('\n').take(begin.line - 1).map(_.size + 1).sum + begin.column - 1
          val stop = data.split('\n').take(end.line - 1).map(_.size + 1).sum + end.column
          val definition = declaration.toString
          if (getDocgenVersion(data, start).getOrElse(-1) < version_class) {
            println(s"Class Definition: $definition")
            val comment = getDocumentationComment(definition, spaces(begin.column))
            edits ++= List((start, injectDocgenVersion(comment, version_class)))
          }

          super.visit(n, arg)
        }

        override def visit(n: MethodDeclaration, arg: Void): Unit = {

          val begin = n.getRange.get().begin
          val end = n.getRange.get().end
          val start = data.split('\n').take(begin.line - 1).map(_.size + 1).sum + begin.column - 1
          val stop = data.split('\n').take(end.line - 1).map(_.size + 1).sum + end.column
          if (getDocgenVersion(data, start).getOrElse(-1) < version_class) {
            val declaration = n.clone()
            removeComments(declaration)
            declaration.getChildNodes.asScala.toArray.foreach({
              case node : BlockStmt if node.toString.size > maxMethodBodySize => declaration.remove(node)
              case _ => // Ignore
            })
            val definition = declaration.toString
            if ("""(?s)(?<![\w\d])private(?![\w\d])""".r.findFirstMatchIn(definition).nonEmpty) {
                println(s"Private Method")
              } else if ("""@Override(?!\w)""".r.findFirstMatchIn(definition).nonEmpty) {
                println(s"Overridden Method")
              } else {
                println(s"Method Definition: $definition")
                val comment = getDocumentationComment(definition, spaces(begin.column))
                edits ++= List((start, injectDocgenVersion(comment, version_class)))
              }
            }
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

  private def removeComments(declaration: MethodDeclaration) = {
    declaration.setJavadocComment("")
    declaration.setComment(null)
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


