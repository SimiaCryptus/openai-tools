package com.simiacryptus.openai

import com.fasterxml.jackson.databind.node.ObjectNode
import com.simiacryptus.openai._CharneskiCustody.root
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import org.apache.commons.io.FileUtils
import org.apache.commons.text.WordUtils
import org.apache.commons.text.similarity.LevenshteinDistance
import org.eclipse.jetty.server.handler.DefaultHandler
import org.eclipse.jetty.server.{Request, Server}

import java.awt.Desktop
import java.io.File
import java.net.URI
import java.util.Properties
import java.util.regex.Pattern
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.{collectionAsScalaIterableConverter, enumerationAsScalaIteratorConverter, mapAsScalaMapConverter}
import scala.util.Random.shuffle
import scala.xml.Elem

trait TextfileWrangler extends Wrangler {

  def additionalTermList: String

  def wordFile: String

  def maxFiles: Int

  def people: Seq[PersonInfo]

  def fixups: Seq[FuzzyStringFixer]

  def verbatimAtom(verbatimText:String) = new xml.Atom(verbatimText) {
    override def buildString(sb: StringBuilder): StringBuilder =
      sb.append(verbatimText)
  }

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
        case "/search" => {
          val str = request.getParameter("q")
          val matches = searchFn_doc(str)
          val results = (for (file <- files) yield {
            (FileUtils.readFileToString(file, "UTF-8"), file)
          }).filter(t => matches(t._1))
          response.setContentType("text/html;charset=UTF-8")
          val writer = response.getWriter
          writer.print(renderSearch(results, str).toString())
          writer.flush()
          response.setStatus(HttpServletResponse.SC_OK)
        }
        case "/questions" => {
          if(request.getParameterNames.asScala.contains("q")) {
            val str = request.getParameter("q")
            val number = request.getParameterMap.asScala.get("n").getOrElse("10").toString.toInt
            response.setContentType("text/html;charset=UTF-8")
            val writer = response.getWriter
            writer.print(renderQuestionaire(str, number).toString())
            writer.flush()
            response.setStatus(HttpServletResponse.SC_OK)
          } else {
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
                    window.open('/questions?q='+ searchBox.value);
                    }}
                    }});
                    }});
                  </script>
                </head>
                <body>
                  <h1>Recursive Questionaire Generator</h1>
                </body>
              </html>.toString())
            writer.flush()
            response.setStatus(HttpServletResponse.SC_OK)
          }
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

  def main(args: Array[String]): Unit = {
    val ordered: Array[File] = shuffle(files).filter(_.length() > 0).take(maxFiles).sortBy(_.getAbsolutePath).toArray

    lazy val fixers = refineFixers(fixups ++ people.flatMap(_.getFixers()), wordList_nearContent).toArray
    //    lazy val fixers = refineFixers(fixups ++ people.flatMap(_.getFixers()), Set.empty)

    lazy val allLines = files.map(FileUtils.readFileToString(_, "UTF-8")).flatMap(_.split('\n'))
    lazy val globalHeaders: Array[String] = findClusters(allLines, 4).filter(_.size > 16).flatten.distinct
    lazy val watershedFn = token_watershed(allLines)

    lazy val addresses: Map[List[String], Int] = findAddresses(allLines)
    lazy val names: Map[List[String], Int] = findProperNames(allLines)
    //println(addresses.map(t=>t._1.mkString("; ")->t._2).toList.sortBy(-_._2).map(t=>s"Address: ${t._1} (${t._2} occurrances)").mkString("\n"))
    //println(names.toList.sortBy(-_._2).sortBy(_._1(3)).map(t=>t._1.mkString("; ")->t._2).map(t=>s"Name: ${t._1} (${t._2} occurrances)").mkString("\n"))
    lazy val nameFixers = getNameFixers(names, addresses)

    val properTerms = (names.keys ++ addresses.keys).flatten.flatMap(_.split("\\s+")).map(_.toLowerCase).toSet.toBuffer
    properTerms ++= additionalTermList.split(',').map(_.trim.toLowerCase)

    def normalizeWord(str: String) = {
      str.toLowerCase.stripSuffix("'s").stripSuffix("'").stripPrefix("'")
    }

    lazy val allTerms = (lcwords ++ properTerms).toArray.toSet

    def isKnown(str: String) = allTerms.contains(normalizeWord(str))

    lazy val fileTypeData: Map[File, Map[String, List[String]]] = (for (file <- ordered) yield {
      val tokens = FileUtils.readFileToString(file, "UTF-8").split("\n").flatMap(toTokens).toList
      file -> tokens.groupBy({
        case (str, WordChar) =>
          if (isKnown(str)) {
            "known word"
          } else if (str.size < 3) {
            "small junk"
          } else if (str.split('-').forall(isKnown)) {
            "hyphenated"
          } else {
            "unknown word"
          }
        case (_, charType) => charType.getClass.getSimpleName.stripSuffix("$")
      }).mapValues(_.map(_._1))
    }).toMap

    def sort(m: Map[String, List[String]]) = {
      m.get("unknown word").map(_.distinct.size).getOrElse(0).toDouble / m.values.map(_.size).sum
    }

    //for((file, fileTypeData) <- fileTypeData; (tokenType, count) <- fileTypeData) {    }
    lazy val tokenSets = fileTypeData.mapValues(_.mapValues(_.map(_.toLowerCase))).toList
      .sortBy(t => sort(t._2))

    lazy val goodFiles = tokenSets.filter(t => t._2.get("unknown word").map(_.distinct.size).getOrElse(0) < 4).map(_._1)

    //println(goodFiles.flatMap(fileTypeData(_).get("unknown word").toList.flatten).groupBy(x=>x).mapValues(_.size).toList.sortBy(-_._2).map(t=>t._1).mkString(","))
    //goodFiles.flatMap(fileTypeData(_).get("unknown word").toList.flatten).groupBy(x=>x).mapValues(_.size).toList.sortBy(-_._2).map(t=>t._1 + " = " + t._2).foreach(println)

    //    for((file, tokenSet) <- tokenSets) {
    //      println(
    //        s"""${file.getAbsolutePath}
    //           |\t${sort(tokenSet)} - ${tokenSet.get("unknown word").toList.flatten.distinct.sorted.mkString(",")}""".stripMargin)
    //    }

    val codec = GPT2Codec.getCodec_345M

    reduceFiles(ordered, minFileSize = 256, clusterRadius = 10)
    reduceFiles(ordered, minFileSize = 16, clusterRadius = 1)
    tiff2png(files)

    var totalSize = 0
    for (file <- ordered) {
      var data = FileUtils.readFileToString(file, "UTF-8")
      val n = codec.encode(data).size()
      totalSize = totalSize + n
      println(s"File: ${file.getName} $n tokens ($totalSize total)")



      //data = charFilter(data)
      //data = repunctuate(data)
      //data = removeLeftMarginJunk(data)
      //data = analyzeSentences(data)

      /**
       * This code splits the input data string into lines, then
       * for each line:
       * - trims it
       * - applies of series of regular expressions, removing all found sequences
       * - trims the resulting string again
       *
       * Finally, it joins all the processed lines back together
       * into a single string, and trims it one last time.
       */
      //            data = (for (line <- data.split('\n')) yield {
      //              List[Regex](
      //                /**
      //                 * This regex matches a sequence of one or more digits
      //                 * that is not followed by a period.
      //                 */
      //                //"""^\d+(?!\.)""".r
      //              ).foldLeft(line.trim)((str, regex) => {
      //                regex.replaceAllIn(str, "")
      //              }).trim
      //            }).mkString("\n").trim

      /**
       * This code takes in a string of data, splits it into lines, and then yields a new string with certain lines removed.
       * The lines that are removed are those where:
       * - There are fewer than 3 words in the line that are more than 2 letters long
       * - And, the proportion of non-whitespace characters to total characters in the line is less than 0.9
       */
      //      data = (for (line <- data.split('\n')) yield {
      //        lazy val words = wordList.filter(_.size > 2).map(_.toLowerCase).filter(w => line.toLowerCase.contains(w)).toList.sortBy(-_.size)
      //        lazy val jibberish = words.foldLeft(line.toLowerCase)((line, word) => line.replaceAllLiterally(word, "")).replaceAllLiterally("""\s+""", "")
      //        val nonwhitespace = line.replaceAllLiterally("""\s+""", "")
      //        if (words.size < 3 && (jibberish.length.toDouble / nonwhitespace.length) > 0.9) {
      //          ""
      //        } else {
      //          line
      //        }
      //      }).mkString("\n").trim

      //      data = fixers.foldLeft(data)((data, finder) => finder(data)) // Apply Fixers

      //data = nameFixers.foldLeft(data)((data, finder) => finder(data)) // Apply Fixers

      //      data = fixWordlist_data((wordList ++ fixers.flatMap(_.pattern.split("\\s+"))).flatMap(f => List(f, f + "'s")).flatMap(f => List(
      //        f.toLowerCase(),
      //        f.toUpperCase(),
      //        WordUtils.capitalize(f.toLowerCase())
      //      )), data)


      //      data = data.replaceAll("""(?i)(?<!\w)mr\.""", "mr")
      //      data = data.replaceAll("""(?i)(?<!\w)mrs\.""", "mrs")
      //      data = data.replaceAll("""(?i)(?<!\w)ms\.""", "ms")
      //      data = data.replaceAll("""(?si)(?<!\w)i\.\s*e\.""", "ie")
      //      data = data.replaceAll("""(?si)(?<!\w)e\.\s*g\.""", "eg")
      //      data = data.replaceAll("""(?s)\n{3,}""", "\n\n")
      //      data = data.replaceAll("""(?s)(\s+)\|(\s+[a-z])""", "$1I$2")
      //      data = data.replaceAll("""(?s)([a-z]\s+)\|(\s+)""", "$1I$2")
      //      data = data.replaceAll("""(?si)Cham\s*eski""", "Charneski")
      //data = collectParagraphs(data)

      //      data = watershedFn(data)
      //data = removeLines(data, findClusters(data.split('\n'), 2).filter(_.size > 3).toList.flatten.distinct)
      //data = removeLines(data, globalHeaders)

      //data = anonymizer.editLines(line => smartFix(fix_openAI)(line))(data)
      //data = anonymizer.edit(fixViaTokenWindow(fix_openAI)(_))(data)

      //data = splitSentences(data)
      //      data = data.replaceAll("""(?s)\n{3,}""", "\n\n")

      //      data = data.split('\n').map(_.trim).mkString("\n").trim

      write(file, data)
    }

    searchServer().join()
  }

  private def tiff2png(files: Seq[File]) = {
    (for (file <- files.par) yield {
      val tiff = new File(file.getAbsolutePath.stripSuffix(".txt"))
      val png = new File(file.getAbsolutePath.stripSuffix(".txt") + ".png")
      if (!png.exists()) {
        println("Converting " + tiff)
        import javax.imageio.ImageIO
        try {
          val bufferedImage = ImageIO.read(tiff)
          ImageIO.write(bufferedImage, "png", png)
        } catch {
          case e: Throwable => e.printStackTrace()
        }
      }
      tiff -> png
    }).toArray.toMap
  }

  def searchFiles(str: String): Unit = {
    val matches = searchFn_doc(str)
    val results = (for (file <- files) yield {
      (FileUtils.readFileToString(file, "UTF-8"), file)
    }).filter(t => matches(t._1))
    open("search_", renderSearch(results, str))
  }

  private def open(prefix: String, html: Elem) = {
    val tempFile = File.createTempFile(prefix, ".html")
    FileUtils.write(tempFile, html.toString(), "UTF-8")
    Desktop.getDesktop.open(tempFile);
  }

  private def searchFn_doc(str:String) = {
    val rules = searchFn_rules(str)
    (s: String) => rules.map(_(s)).reduce(_ && _)
  }

  private def searchFn_rules(str: String) = {
    str.split("\\s+").map(term => {
      val not = term.startsWith("!")
      val pattern = (s"(?i)(?<!\\w)$term(?!\\w)").r
      (s: String) => pattern.findFirstIn(s).isEmpty == not
    })
  }

  private def searchFn_line(str:String) = {
    val rules = searchFn_rules(str)
    (s: String) => rules.map(_(s)).reduce(_ || _)
  }

  private def renderQuestionaire(str: String, number : Int = 10): Elem = {

    def tenq(s:String) = {
      val response = openAI("text-davinci-002").complete(CompletionRequest(
        s"""$number detailed questions to answer "$s":
           |
           |1.""".stripMargin
      ))
      ("1." + response.choices.head.text).split("\n")
        .map(_.trim).filter(_.nonEmpty)
        .map(line => line.split('.').drop(1).mkString("."))
    }

    <html>
      <body>
        <h1>{str}</h1>
        <ol>
          {tenq(str).par.map(s1 =>
          <li>
            <div>
              {s1}
            </div>
            <div>
              <ol>
                {tenq(s1).map(s2 => <li>{s2}</li>)}
              </ol>
            </div>
          </li>
          ).toArray}
        </ol>
      </body>
    </html>
  }

  private def renderSearch(results: List[(String, File)], str: String, simpleLinks : Boolean = true): Elem = {
    val matches = searchFn_line(str)
    <html>
      <head>
        <script>{verbatimAtom(
          """
            |function imageHandler(imgUrl) {
            |    var imageDiv = document.createElement('div');
            |    imageDiv.style.position = 'fixed';
            |    imageDiv.style.top = '0';
            |    imageDiv.style.left = '0';
            |    imageDiv.style.width = '100%';
            |    imageDiv.style.height = '100%';
            |    imageDiv.style.backgroundColor = 'rgba(0, 0, 0, 0.5)';
            |    imageDiv.style.zIndex = '9999';
            |    imageDiv.style.textAlign = 'center';
            |    var image = document.createElement('img');
            |    image.src = imgUrl;
            |    image.style.maxWidth = '90%';
            |    image.style.maxHeight = '90%';
            |    image.style.marginTop = '100px';
            |    image.style.marginLedft = '100px';
            |    imageDiv.appendChild(image);
            |    document.body.appendChild(imageDiv);
            |    imageDiv.onclick = function() {
            |      document.body.removeChild(imageDiv);
            |    };
            |    document.onkeydown = function(e) {
            |      if (e.keyCode === 27) {
            |        document.body.removeChild(imageDiv);
            |      }
            |    };
            |}
            |""".stripMargin)}</script>
      </head>
      <body>
        <h1>Search for "
          {str}
          "</h1>{for ((data, file) <- results) yield {
        val link = (if (simpleLinks) {
          "/file/" + root.toPath.relativize(file.toPath).toString
        } else {
          file.toURI().toASCIIString
        }).replace('\\','/')
        val waCourtDoc = """(\d+)_(\d\d\d\d\.\d\d\.\d\d)_(.*?)_Page_(\d+).*""".r

        <div>
          <h4>
            <a href={link}>
              {file.getName}
            </a>
            -
            <a onclick={ s"""imageHandler('${link.stripSuffix(".txt") + ".png"}')""" }>(image)</a>
            -
            <a href={ link.replaceAll("""_Page_\d+\.tif\.txt$""", ".pdf") } target="_blank">(pdf)</a>
          </h4>
          {file.getName match {
          case waCourtDoc(id, date, title, page) =>
            <div>
              <div>WA Filing # {id}</div>
              <div>Date: {date}</div>
              <div>Title: {title}</div>
              <div>Page: {page}</div>
            </div>
          case _ => <!-- No match -->
          }}
          {data.split("""[\n\.]""").filter(matches).map(line => <div>{line}</div>)}
        </div>
      }}
      </body>
    </html>
  }

  private def reduceFiles(ordered: Array[File], minFileSize: Int, clusterRadius: Int) = {
    val allData: Map[String, Array[(Int, String, File)]] = (for ((file, idx) <- ordered.zipWithIndex) yield {
      (idx, FileUtils.readFileToString(file, "UTF-8"), file)
    }).groupBy(_._2).filter(_._1.length > minFileSize)
    val fileClusters: Array[Array[File]] = findClusters_raw(
      allData.keys.toList, clusterRadius
    ).map(_.flatMap(allData(_).map(_._3)).distinct.toArray)
      .filter(_.length > 1)
    for (fileCluster <- fileClusters) {
      println(s"File Cluster: ${fileCluster.length} files")
      val sorted = fileCluster
        .sortBy(_.getAbsolutePath)
        .sortBy(-_.getAbsolutePath.length)
      val primary = sorted.head
      println(s"\t${primary.getAbsolutePath}")
      for (file <- sorted.tail) {
        println(s"\t${file.getAbsolutePath} (DELETED)")
        file.delete()
      }
      println(s"")
    }
  }

  def repunctuate: String => String = anonymizer.edit((data: String) => {
    repunctuate_raw(
      data = data,
      ada = openAI("text-ada-001"),
      insertTokens = Map(
        "." -> -1e-1,
        "\n" -> -1e-1,
        "\n\n" -> -1e-1
      ),
      windowSize = 128)
  })

  def repunctuate_raw(data: String, ada: OpenAIAPI, insertTokens: Map[String, Double], windowSize: Int): String = {
    var workingData = data
    workingData = workingData.replace('\n', ' ')
      //.replaceAll("""(?<=[a-z])\.(?!\w)""", " ")
      .replaceAll("\\s{2,}", " ")
    val compacted = workingData
    val reformatted = new StringBuffer()

    def nextBatch = toTokens(workingData).map(_._1).toArray.toBuffer.take(windowSize).mkString

    def insertChoices(topChoices: ObjectNode) = insertTokens.keys
      .flatMap(t => Option(topChoices.get(t)).map(t -> _.asDouble())).toMap
      .filter(t => t._2 > insertTokens(t._1))

    while (workingData.nonEmpty) {
      val completion = ada.complete(CompletionRequest(nextBatch, logprobs = Option(3), max_tokens = 0, echo = true))
      if (completion.error.nonEmpty) {
        println("Error: " + completion.error.get.message)
        reformatted.append(workingData)
        workingData = ""
      } else {
        val logProbs = completion.choices.headOption.map(_.logprobs.get)
        val sequence = logProbs.map(_.tokens.zip(logProbs.get.top_logprobs)
          .zipWithIndex.map(t => (t._1._1, t._1._2, t._2))).getOrElse(Array.empty)
        lazy val splitPoint_max = sequence.filter(_._2 != null).filter(t => !insertTokens.contains(t._1))
          .maxBy(t => {
            val map = insertChoices(t._2)
            if (map.isEmpty) {
              Double.NegativeInfinity
            } else {
              map.values.max
            }
          })
        lazy val splitPoint_first = sequence.filter(_._2 != null)
          .filter(t => !insertTokens.contains(t._1))
          .filter(t => insertChoices(t._2).nonEmpty)
          .headOption.getOrElse((null, null, -1))

        def splitPoint = splitPoint_first

        lazy val splitTokens = insertChoices(sequence(splitPoint._3)._2)
        if (splitPoint._3 < 0 || splitTokens.isEmpty) {
          val string = sequence.map(_._1).mkString
          require(workingData.startsWith(string))
          workingData = workingData.stripPrefix(string)
          reformatted.append(string)
        } else {
          val string = sequence.take(splitPoint._3).map(_._1).mkString
          require(workingData.startsWith(string))
          workingData = workingData.stripPrefix(string)
          reformatted.append(string)
          reformatted.append(splitTokens.maxBy(_._2)._1)
        }
      }
    }
    reformatted.toString.split('\n').map(_.trim).mkString("\n\n")
    //tokens.mkString
  }

  private def findProperNames(allLines: => List[String]) = {
    val nameRegex =
      """(?x)
        |(?<title>(?:(?:Mr?s|Mr|mr?s|mr)?\.?\s+)?)
        |(?<first>[A-Z][a-z]{1,20})[\s]+
        |(?<middle>(?:[A-Z][a-z]{0,10}\.?\s+)?)
        |(?<last>[A-Z][a-z]{1,20}(?:\-[A-Za-z]{1,20}){0,2})""".stripMargin.trim.r
    val names = allLines.flatMap(str => nameRegex.findAllIn(str).matchData.toArray.map(_.subgroups.map(_.trim).toArray.toList)).toArray.toList
      .filter(t => t(1).size > 2 && t(3).size > 2)
      .filter(t => !commonWords.contains(t(1).toLowerCase))
      .filter(t => !commonWords.contains(t(3).toLowerCase))
      .groupBy(x => x).mapValues(_.size).filter(_._2 > 2)
    names
  }

  def charFilter(s: String): String = s.map({
    //      case ' ' | '!' | '"' | '#' | '$' | '%' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' => ' '
    //      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | ':' | ';' | '<' | '=' | '>' | '?' | '@' => ' '
    //      case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' => ' '
    //      case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' => ' '
    case '{' | '[' => '('
    case ']' | '}' => ')'
    case '_' => '_'
    case '\\' => '|'
    case '~' | '—' => '-'
    case '€' | '™' | '¢' | '£' | '¥' | '§' | '©' | '«' | '®' | '°' | '»' | 'é' => ' '
    case '‘' | '’' => '''
    case '“' | '”' => '"'
    case c => c
  })

  private def findAddresses(allLines: => List[String]) = {
    val addressRegex =
      """(?x)
        |(?<street>\d{1,4}\s+\d{0,3}[a-zA-Z]{1,2}[a-zA-Z\s]{1,20}(?:[,\s]Suite \d+)?)[,\s]+
        |(?<city>[a-zA-Z\s]{1,20})[,\s]+
        |(?<state>[A-Z]{2})\s+
        |(?<zip>\d{5}(?:\-\d{4})?)""".stripMargin.trim.r
    val addresses = allLines.flatMap(str => addressRegex.findAllIn(str).matchData.toArray.map(_.subgroups.toList))
      .groupBy(x => x).mapValues(_.size).filter(_._2 > 2)
    addresses
  }

  def removeLines(data: String, filtered: Seq[String]): String = {
    val lines = data.split('\n')
    filtered.distinct.toList.filter(lines.contains(_)).foreach(x => println("Removing Line: " + x))
    if (filtered.nonEmpty) {
      lines.filter(!filtered.contains(_)).mkString("\n").trim
    } else {
      data
    }
  }

  def removeLeftMarginJunk(data: String): String = {
    val lines = data.split('\n').map(_.trim).filter(_.nonEmpty).toList
    if (lines.filter(_.head.isLetter).size < (lines.size * 0.5)) {
      data.split('\n').map(_.trim.replaceAll("^[^a-zA-Z]+", "")).mkString("\n")
    } else {
      data
    }
  }

  def analyzeSentences(data: String): String = {
    val props = new Properties()
    props.setProperty("annotators", "tokenize,ssplit")
    //props.setProperty("ssplit.eolonly", "true")
    val pipeline = new StanfordCoreNLP(props)
    val document = pipeline.processToCoreDocument(data)
    document.sentences().asScala.mkString("\n\n")
  }

  private def userEdit(file: File, lines: List[String]): List[String] = {
    FileUtils.write(file, lines.sorted.mkString("\n"), "UTF-8")
    Desktop.getDesktop.edit(file)
    val result = FileUtils.readFileToString(file, "UTF-8").split('\n').toList
    file.delete()
    result
  }

  private def getNameFixers(names: Map[List[String], Int], addresses: Map[List[String], Int]) = {
    val nameTokens = names.filter(_._2 > 2).flatMap(_._1).map(_.trim.stripSuffix(".")).filter(_.size > 2).toList.distinct.sorted.toArray
    println("Names: " + nameTokens.mkString(", "))
    var properTerms = (nameTokens ++ addresses.map(_._1(1)) ++ addresses.map(_._1(0))).toList.distinct
      .filter(_.size > 5)
      .filter(w => !commonWords.contains(w.toLowerCase))
    properTerms = userEdit(new File("proper_terms.txt"), properTerms)
    val nameFixers = properTerms.map(w => w -> (w match {
      case _ if w.size <= 5 => 0
      case _ if w.size <= 9 => 1
      case _ if w.size <= 14 => 2
      case _ => 3
    })).filter(_._2 > 0).map(t => FuzzyStringFixer(t._1, t._2, properTerms.filter(_ != t._1)))
    nameFixers
  }

  def token_watershed(strings: Seq[String]): String => String = {
    val radius = 2
    val minLength = 4
    val maxLength = 64
    val candidates = strings.flatMap(toTokens)
      .filter(_._2 == WordChar)
      .map(_._1)
      .filter(_.size > minLength)
      .filter(_.size < maxLength)
      .filter(!_.equalsIgnoreCase("EXHIBIT"))
      .groupBy(x => x).mapValues(_.size)

    val distance = new LevenshteinDistance(radius)

    def clusterSeeds(subjects: Iterable[String], objects: Iterable[String]) = {
      println(s"Finding clusters of radius ${radius} among ${subjects.size} x ${objects.size} strings")
      subjects.par.map(l => l -> {
        objects.filter(l2 => distance(l, l2) != -1).toArray
      }).toArray.map(t => (List(t._1) ++ t._2).toArray)
    }

    val clusters0 = clusterSeeds(candidates.keys, candidates.keys).sortBy(-_.size).toBuffer
    val usedWords = new mutable.HashSet[String]()
    val clusters1 = new ArrayBuffer[(String, List[String])]()
    println(s"Aggregating ${clusters0.size} clusters")
    while (clusters0.nonEmpty) {
      val cluster = clusters0.remove(0).filter(!usedWords.contains(_))
      if (cluster.size > 1) {
        val target = cluster
          .filter(str => lcwords.contains(str.toLowerCase))
          .sortBy(x => -candidates(x))
          .headOption
        for (target <- target) {
          val sources = cluster
            .filter(str => !lcwords.contains(str.toLowerCase))
            .filter(str => !str.startsWith(target) && !target.startsWith(str) && !target.equalsIgnoreCase(str))
            .filter(str => {
              ((candidates(str) < 5) && (candidates(str) < (0.8 * candidates(target)))) ||
                ((candidates(str) < 500) && (candidates(str) < (0.5 * candidates(target))))
            })
            .toList
          if (sources.nonEmpty) {
            usedWords += target
            usedWords ++= sources
            clusters1 += (target -> sources.toArray.toList)
          }
        }
      }
    }
    val clusters2 = clusters1.toArray.toMap
    var replacements = clusters2.flatMap(t => t._2.map(_ -> t._1)).toArray.toMap
    while (replacements.values.find(replacements.contains(_)).nonEmpty) {
      replacements = replacements.mapValues(str => replacements.get(str).getOrElse(str)).toArray.toMap
    }
    println(s"${clusters2.size} Replacement clusters: ${clusters2.keys.mkString(", ")}")

    (str: String) => {
      toTokens(str).map({
        case (word, WordChar) if replacements.contains(word) =>
          replacements(word)
        case (other, _) =>
          other
      }).mkString
    }
  }

  /**
   * This code splits a string of text into sentences.
   * It does this by looking for capital letters, which it uses as an indication of the beginning of a sentence.
   * It also looks for periods, question marks, and exclamation points, which it uses as an indication of the end of a sentence.
   * It ignores any other punctuation. It also ignores any digits that may be in the string of text.
   *
   * @param data
   * @return
   */
  def splitSentences(data: String): String = {
    val lineInput = data.split('\n').toBuffer
    val lineOutput = new ArrayBuffer[String]()
    val sb = new StringBuffer()
    while (lineInput.nonEmpty) {
      val line = lineInput.remove(0)
      if (line.filter(c => (c.isWhitespace || c.isLetterOrDigit)).isEmpty) {
        // Ignore Line
      } else if ((line.count(_.isLower).toDouble / line.size) < 0.5) {
        if (sb.length() > 0) lineOutput += sb.toString
        lineOutput += ""
        lineOutput += line.trim
        lineOutput += ""
        sb.delete(0, sb.length())
      } else {
        if (line.nonEmpty) sb.append(" " + line)
        var continueLoop = true
        while (continueLoop) {
          val (scannerState, sentenceLength) = sb.toString.toCharArray.foldLeft((0, 0))((t, c) => c match {
            //case '.' | '?' | '!' if (t._1 < 0) => (t._1, t._2 + 1)
            case _ if (t._1 == -2) => t
            case d if d.isDigit && (t._1 < 0 || t._1 == 1) => (1, t._2 + 1)
            case _ if (t._1 == 1) => (-1, t._2 + 1)
            //            case '"' if t._1 == 0 => (2, t._2 + 1)
            //            case '"' if t._1 == 2 => (0, t._2 + 1)
            //            case '(' if t._1 == 0 => (3, t._2 + 1)
            //            case ')' if t._1 == 3 => (0, t._2 + 1)
            case '.' | '?' | '!' if (t._1 == 0 && (t._2 + lineOutput.size) > 16) => (-1, t._2 + 1)
            case ' ' if (t._1 == -1) => (-2, t._2)
            case _ => (t._1, t._2 + 1)
          })
          if (scannerState < 0) {
            lineOutput += sb.substring(0, sentenceLength).trim
            sb.delete(0, sentenceLength)
          } else {
            continueLoop = false;
          }
        }
      }
    }
    lineOutput += sb.toString.trim
    sb.delete(0, sb.length())

    lineOutput.mkString("\n")
  }

  /**
   * This method collects paragraphs from the given data string.
   * A paragraph is defined as a sequence qualifying
   * continuations of lines.
   *
   * @param data the string to collect paragraphs from
   * @return the collected paragraphs
   */
  def collectParagraphs(data: String): String = {
    val lineInput = data.split('\n').toBuffer
    val lineOutput = new ArrayBuffer[String]()
    while (lineInput.nonEmpty) {
      var line = lineInput.remove(0)
      var lines = 1
      if (wordsInString(line).size > 10) {
        while (lineInput.nonEmpty && follows(line, lineInput.head)) {
          val nextLine = lineInput.remove(0)
          line = line + (if (nextLine.isEmpty) {
            nextLine
          } else {
            lines = lines + 1
            " " + nextLine
          })
        }
      }
      lineOutput += line
      if (lines > 1) lineOutput += ""
    }
    lineOutput.mkString("\n")
  }


  /**
   * This function checks if a given string "left" is followed by another given string "right".
   * It returns true if "right" is not empty, if there is more than one word in "right",
   * and if more than 80% of the characters in "right" are lowercase letters or punctuation marks,
   * and false otherwise.
   */
  def follows(left: String, right: String): Boolean = {
    if (right.nonEmpty) {
      val newWords = wordsInString(right)
      if (newWords.size > 1) {
        val punctuation = Array('.', ',', ' ')
        val nonHeaderChars = right.filter(c => c.isLower || punctuation.contains(c)).size
        if ((nonHeaderChars.toDouble / right.length) > 0.8) {
          true
        } else {
          false
        }
      } else {
        false
      }
    } else {
      false
    }
  }

  /**
   * This function takes in a string and returns an array of words that are found in the string.
   * It first converts the string to lowercase, then finds all the words in the string that are also
   * in the lowercase wordlist_raw array. It sorts the words by length (longest to shortest) and then
   * removes them from the string one by one.
   */
  def wordsInString(line: String): Array[String] = {
    val lowerCase = line.toLowerCase
    val wordsInLine = wordlist_raw.par.filter(_.size > 1)
      .map(_.toLowerCase).filter(str => lowerCase.contains(str))
    var _line = lowerCase
    wordsInLine.toList.sortBy(-_.size).par.flatMap(w => {
      val buffer = new ArrayBuffer[String]()
      while (_line.contains(w)) {
        _line = _line.replaceFirst("(?i)" + Pattern.quote(w), "")
        buffer += w
      }
      buffer.toArray.toList
    }).toArray
  }

  def openAI(model: String): OpenAIAPI

  def fixViaTokenWindow(
                         fn: String => String,
                         max_word_validity: Double = 0.8,
                         lookbackContext: Int = 4
                       ): String => String = (data: String) => {
    val tokenString = toTokens(data).toBuffer
    val tokenString2 = new ArrayBuffer[(String, CharType)]()
    try {
      while (tokenString.nonEmpty) {
        val (nextToken, charType) = tokenString.remove(0)
        val isWord = charType == WordChar
        if (isWord) {
          val tokenString3 = new ArrayBuffer[(String, CharType)]()
          if (spellcheck.contains(nextToken.toLowerCase)) {
            tokenString2 ++= List((nextToken, charType))
          } else {
            tokenString3 ++= List((nextToken, charType))

            def badTerms = tokenString3.filter(_._2 == WordChar).filter(t => !spellcheck.contains(t._1.toLowerCase))

            while (tokenString.nonEmpty && (tokenString3.filter(_._2 == WordChar).filter(t => spellcheck.contains(t._1.toLowerCase)).size.toDouble / tokenString3.filter(_._2 == WordChar).size.toDouble < max_word_validity)) {
              tokenString3 ++= List(tokenString.remove(0))
            }
            (0 until lookbackContext).foreach(_ => tokenString3.insert(0, tokenString2.remove(tokenString2.size - 1)))
            var blurb = tokenString3.map(_._1).mkString
            blurb = fn(blurb)
            tokenString2 ++= toTokens(blurb)
          }
        } else {
          tokenString2 ++= List((nextToken, charType))
        }
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        tokenString2 ++= tokenString
    }
    tokenString2.map(_._1).mkString
  }

  sealed trait CharType {
    def apply(c: Char): Boolean
  }

  object WordChar extends CharType {
    override def apply(c: Char): Boolean = {
      c.isLetter || Set('-', '\'').contains(c)
    }
  }

  object NumberChar extends CharType {
    override def apply(c: Char): Boolean = c.isDigit
  }

  object SpaceChar extends CharType {
    override def apply(c: Char): Boolean = c.isWhitespace
  }

  object AnyChar extends CharType {
    override def apply(c: Char): Boolean = List(WordChar, NumberChar, SpaceChar).forall(!_ (c))
  }

  def toTokens(data: String): List[(String, CharType)] = {
    /**
     * This code converts a string of data into a list of words and spaces.
     * First, the data string is converted into a list of characters.
     * Then, the list of characters is converted into a buffer.
     * A buffer is a data structure that allows characters to be added or removed.
     * Next, the code defines three functions: nextSpace, nextWord, and nextSpecials.
     * The nextSpace function takes the characters from the buffer until it finds a non-whitespace character.
     * The nextWord function takes the characters from the buffer until it finds a non-letter character.
     * The nextSpecials function takes the characters from the buffer until it finds a whitespace or letter character.
     * Finally, the code creates a new array buffer.
     * The array buffer is a data structure that allows arrays to be added or removed.
     * The code then adds the words, spaces, and special characters to the array buffer.
     * The code then filters the array buffer to remove any empty strings.
     */
    val dataBuffer = data.toCharArray.toList.toBuffer

    def nextType = List(WordChar, NumberChar, SpaceChar, AnyChar).filter(_ (dataBuffer.head)).head

    def takeWhile(charType: CharType) = dataBuffer.takeWhile(charType(_)).mkString

    val tokenString = new ArrayBuffer[(String, CharType)]()
    while (dataBuffer.nonEmpty) {
      tokenString ++= List((takeWhile(nextType), nextType))
      dataBuffer.remove(0, tokenString.last._1.length)
    }
    tokenString.toArray.toList
  }

  def smartFix(fixFn: String => String, chunkSizeLimit: Int = 1000): String => String = (line: String) => {
    if (line.size < 8) {
      line
    } else {
      val lowerCase = line.toLowerCase
      val words = wordlist_raw.par.filter(_.size > 1)
        .map(_.toLowerCase)
        .filter(str => lowerCase.contains(str))
        .toList
      if (words.size < 8) {
        line
      } else {
        def fixIfWrong(line: String): String = {
          val remaining = words.distinct.sortBy(-_.size).foldLeft(List(line))((list, word) => {
            list.flatMap(_.split("(?i)" + word))
          }).toArray
          val badTokens = remaining.filter(t => !t.matches("""[\s\.]+"""))
          if (badTokens.map(_.trim.filter(_.isLetter).size).sum > 6) {
            if (line.length > chunkSizeLimit) {
              val allWords = line.split(' ')
              val split = allWords.grouped((allWords.size.toDouble / (line.length.toDouble / chunkSizeLimit).ceil).ceil.toInt).map(_.mkString(" "))
              split.map(fixIfWrong).mkString(" ")
            } else {
              fixFn(line)
            }
          } else {
            line
          }
        }

        fixIfWrong(line)
      }
    }
  }

  def fix_openAI: String => String = openAI("text-davinci-002").xmlFN(
    instruction = "Correct OCR errors and typos in the following text.",
    inputTag = """text""",
    inputAttr = Map("type" -> "input"),
    outputTag = """text""",
    outputAttr = Map("type" -> "output")
  )

  lazy val anonymizer: Anonymizer = Anonymizer(people, files.map(file => FileUtils.readFileToString(file, "UTF-8")))

  lazy val contentLines: Array[String] = files.flatMap(file => FileUtils.readFileToString(file, "UTF-8").split("\\n")).distinct.sorted.toArray

  lazy val contentWords: Array[String] = files.flatMap(file => FileUtils.readFileToString(file, "UTF-8").split("\\s+")).distinct.sorted.toArray

  lazy val lcwords: Set[String] = wordlist_raw.map(_.toLowerCase)

  lazy val allWords: Set[String] = FileUtils.readFileToString(new File(wordFile), "UTF-8").split('\n').distinct.sorted.toSet
  lazy val commonWords: Set[String] = allWords.filter(w => w == w.toLowerCase) ++ Set("winter", "court", "other", "mother", "family", "player", "father", "great", "story", "state", "states", "center", "transit", "little", "website", "easter", "international", "school", "records", "parent", "north", "south", "black", "white", "service", "member", "division", "street", "server")

  lazy val wordlist_raw: Set[String] = FileUtils.readFileToString(new File(wordFile), "UTF-8").split('\n').sorted.toSet ++ Set(
    // Don't Add Names!
    "litem", "videogame", "FALSE", "evals", "emails", "mailto", "http", "https"
  ).map(_.toLowerCase)

  /**
   * This function returns a list of words from the default word list.
   * The list is filtered so that it only contains words that are similar to the content words
   * (i.e. they have a Levenshtein distance of 1 or less).
   */
  lazy val wordList_nearContent: Set[String] = {
    val defaultWords = wordlist_raw
    val contentWords = this.contentWords
    defaultWords.par.filter(w => contentWords.contains(w) || contentWords.exists(a => {
      lazy val dist = new LevenshteinDistance(1).apply(w, a)
      (w.length - a.length).abs <= 1 && (dist == 0 || dist == 1)
    })).toArray.toSet
  }

  lazy val spellcheck: mutable.HashSet[String] = {
    val hashSet = new mutable.HashSet[String]()
    hashSet ++= wordlist_raw.map(_.toLowerCase)
    hashSet ++= fixups.map(_.pattern.toLowerCase)
    hashSet ++= people.flatMap(p => List(p.firstName, p.lastName, p.middleName).filter(_.size > 3) ++ p.titles).map(_.toLowerCase)
    hashSet ++= Anonymizer.firstNames.map(_.toLowerCase)
    hashSet ++= Anonymizer.lastNames.map(_.toLowerCase)
    hashSet
  }

  def files: List[File]

  /**
   * Replaces words in `data` with words from `wordList`, using `cache` to store
   * previously-checked words.
   */
  def fixWordlist_data(wordList: Set[String], data: String, cache: mutable.HashMap[String, String] = new mutable.HashMap[String, String]): String = {
    data.split("\\n").map(
      line => line.split('.').map(
        sentence => sentence.split("\\s+").map(
          word => {
            val newWord = fixWordlist_word(wordList, word, cache)
            if (word != newWord) println(s"Fixed $word with $newWord")
            newWord
          }).mkString(" ")
      ).mkString(".")
    ).mkString("\n")
  }

  /**
   * This function takes in a word list, a word, a cache, a maximum distance, and an adjoint size.
   *
   * If the word list contains the word, or if the word is less than three letters long,
   * or if the word has less than three letters when all non-letter characters are removed,
   * then the word is returned as-is.
   *
   * If the cache contains the word, then the cached version of the word is returned.
   *
   * Otherwise, a series of possible fixes are considered, based on the Levenshtein distance
   * between the word and the words in the word list.
   *
   * If no possible fixes are found, then the word is returned as-is.
   * Otherwise, the closest match is returned.
   *
   */
  def fixWordlist_word(wordList: Set[String],
                       word: String,
                       cache: mutable.HashMap[String, String],
                       maxDistance: Int = 1,
                       adjointSize: Int = 3): String = {
    if (wordList.contains(word) || word.size < 3 || word.filter(_.isLetter).size < 3) {
      word
    } else if (cache.contains(word)) {
      cache(word)
    } else {
      val result = {
        val nonLetterPrefix = word.takeWhile(!_.isLetter).mkString
        val nonLetterSuffix = word.reverse.takeWhile(!_.isLetter).reverse.mkString
        lazy val stripped = word.stripPrefix(nonLetterPrefix).stripSuffix(nonLetterSuffix)
        if (nonLetterPrefix.nonEmpty || nonLetterSuffix.nonEmpty) {
          nonLetterPrefix + fixWordlist_word(wordList, stripped, cache) + nonLetterSuffix
        } else {
          val distance = new LevenshteinDistance(maxDistance)
          val possibles = wordList.par
            .filter(w => (w.size - word.size).abs <= 1)
            .groupBy(w => Option(distance.apply(w, word).toInt).filter(_ >= 0).getOrElse(Integer.MAX_VALUE))
            .filter(_._1 <= maxDistance).toList
          if (possibles.isEmpty) {
            val prefixes = wordList.par.filter(_.size > adjointSize).filter(word.startsWith)
            if (prefixes.nonEmpty) {
              val prefix = prefixes.toList.sortBy(-_.size).head
              val str = fixWordlist_word(wordList, word.stripPrefix(prefix), cache)
              if (str.head.isLetter) prefix + " " + str else prefix + str
            } else {
              val suffixes = wordList.par.filter(_.size > adjointSize).filter(word.startsWith)
              if (suffixes.nonEmpty) {
                val suffix = suffixes.toList.sortBy(-_.size).head
                val str = fixWordlist_word(wordList, word.stripSuffix(suffix), cache)
                if (str.last.isLetter) str + " " + suffix else str + suffix
              } else {
                word
              }
            }
          } else {
            var (minDistance, closestMatches) = possibles.sortBy(_._1).head
            val maxSize = closestMatches.map(_.size).max
            closestMatches = closestMatches.filter(_.size == maxSize)
            if (1 == closestMatches.size) {
              println(s"Fixed $word with ${closestMatches.head} at distance $minDistance")
              closestMatches.head
            } else {
              println(s"Multiple possible fixes for $word - ${closestMatches.mkString(", ")}")
              word
            }
          }
        }
      }
      cache(word) = result
      result
    }
  }

  /**
   * This function refines a list of fixers by adding lowercase, uppercase, and capitalized versions
   * of the fixer patterns, as well as any words from the provided word list that partially match
   * the fixer patterns.
   */
  def refineFixers(fixers: Seq[FuzzyStringFixer], wordList: Set[String]): Seq[FuzzyStringFixer] = {
    var _fixers = fixers
    _fixers = _fixers.flatMap(f => List(
      f.copy(pattern = f.pattern.toLowerCase),
      f.copy(pattern = f.pattern.toUpperCase()),
      f.copy(pattern = WordUtils.capitalize(f.pattern.toLowerCase()))
    ))
    _fixers = _fixers.map(fixer => {
      val pattern = fixer.pattern
      var newList = _fixers.map(_.pattern).filter(_ != pattern)
      newList = newList ++ wordList.par.filter(_.size > 2).flatMap(x => List(
        x.toLowerCase, x.toUpperCase, WordUtils.capitalize(x.toLowerCase)
      )).filter(word => FuzzyStringFixer.findPartialMatches(pattern, word.toLowerCase, 2, false).nonEmpty)
      newList = newList.filter(w => !pattern.contains(w))
      fixer.copy(avoid = (fixer.avoid ++ newList).distinct.toArray.toList)
    })
    _fixers
  }

  lazy val _levenshteinDistance = new LevenshteinDistance(4)

  def levenshteinDistance(a: String, b: String): Int = {
    val result = _levenshteinDistance.apply(a, b)
    if (result < 0) Integer.MAX_VALUE else result
  }


  /**
   * This function finds the common terms in a list of files.
   *
   * @param ordered a list of files
   * @return a map of common terms and their frequencies
   */
  def commonTerms(ordered: List[File]): Map[List[String], Int] = {
    ordered.flatMap(file => {
      val data = FileUtils.readFileToString(file, "UTF-8")
      val words = data.split("""[^\w]+""").map(_.toLowerCase)
      windowed(words).groupBy(x => x).mapValues(_.size).toList
    }).groupBy(x => x._1).mapValues(_.map(_._2).sum)
      .filter(terms => terms._1.find(x => !wordList_nearContent.contains(x)).nonEmpty)
  }

  def windowed(words: Array[String]): List[List[String]] = {
    if (words.length % 2 == 0) {
      words.grouped(2).toList.map(_.toList).map(_.toList) ++
        words.drop(1).grouped(2).toList.dropRight(1).map(_.toList)
    } else {
      words.grouped(2).toList.dropRight(1).map(_.toList) ++
        words.drop(1).grouped(2).toList.map(_.toList)
    }
  }

  def write(file: File, workingData: String): Unit = {
    try {
      val data = FileUtils.readFileToString(file, "UTF-8")
      if (workingData != data) {
        FileUtils.write(file, workingData, "UTF-8")
        println("Wrote: " + file.getAbsolutePath)
      }
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }

}
