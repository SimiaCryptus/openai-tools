package com.simiacryptus.openai

import com.simiacryptus.openai._CharneskiCustody.root
import org.apache.commons.io.FileUtils
import org.apache.commons.text.similarity.LongestCommonSubsequence
import org.eclipse.jetty.server.handler.DefaultHandler
import org.eclipse.jetty.server.{Request, Server}

import java.awt.Desktop
import java.io.File
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}


trait TableWrangler extends Wrangler {

  class HttpHandler extends DefaultHandler {
    val filePattern = "/file/(.*)".r

    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      request.getPathInfo match {
        case "/" => {
          response.setContentType("text/html;charset=UTF-8")
          val writer = response.getWriter
          writer.print(
            <html>
              <head>
                <script>
                  document.addEventListener("DOMContentLoaded", function() {{
                  var searchBox = document.createElement('input');
                  searchBox.type = 'text';
                  searchBox.id = 'searchBox';
                  document.body.appendChild(searchBox);
                  searchBox.addEventListener('keypress', function(e) {{
                  if (e.keyCode == 13) {{
                  window.open('/search?q='+ searchBox.value);
                  }}
                  }});
                  }});
                </script>
              </head>
              <body>
                <h1>Simple Search Server</h1>
              </body>
            </html>.toString())
          writer.flush()
          response.setStatus(HttpServletResponse.SC_OK)
        }
        case filePattern(filename) => {
          filename.split('.').last match {
            case "txt" => response.setContentType("text/html;charset=UTF-8")
            case "png" => response.setContentType("image/png")
            case "pdf" => response.setContentType("application/pdf")
            case _ => // Do Nothing
          }
          val writer = response.getOutputStream
          writer.write(FileUtils.readFileToByteArray(new File(root, filename)))
          writer.flush()
          response.setStatus(HttpServletResponse.SC_OK)
        }

        case _ => super.handle(target, baseRequest, request, response)
      }
    }
  }

  def searchServer(port: Int = 8090) = {
    import org.eclipse.jetty.server.{Connector, ServerConnector}
    val server = new Server
    val connector = new ServerConnector(server)
    connector.setPort(port)
    server.setConnectors(Array[Connector](connector))
    server.setHandler(new HttpHandler())
    server.start()
    Desktop.getDesktop.browse(new URI(s"http://localhost:$port/"))
    server
  }

  def csvFile : File
  lazy val csvTable = FileUtils.readFileToString(csvFile, "UTF-8").split('\n').map(_.split(","))



  def main(args: Array[String]): Unit = {

    // Account Number,Post Date,Check,Description,Debit,Credit,Status,Balance

    val descriptions = csvTable.map(_ (3))
    val rawClusters = findClusters_raw(descriptions.filter(_.length < 20), 5) ++
      findClusters_raw(descriptions.filter(_.length < 38), 10) ++
      findClusters_raw(descriptions.filter(_.length > 32), 16)
    val summaries = rawClusters.map(_.map(_.asInstanceOf[CharSequence]))
      .map(l => l.reduce((a,b)=>new LongestCommonSubsequence().longestCommonSubsequence(a,b)) -> l).toMap
    for((substr, strings) <- summaries) {
      println(s"$substr:\n\t${strings.mkString("\n\t")}")
    }

    val outputFile = File.createTempFile("out", ".csv")
    FileUtils.write(
      outputFile,
      csvTable.map(row => {
        (Array(summaries.find(_._2.contains(row(3))).headOption.map(_._1).getOrElse("")) ++ row).mkString(", ")
      }).mkString("\n"),
      "UTF-8")
    Desktop.getDesktop.edit(outputFile)

    //searchServer().join()
  }

}
