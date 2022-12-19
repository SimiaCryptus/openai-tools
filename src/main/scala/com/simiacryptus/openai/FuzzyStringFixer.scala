package com.simiacryptus.openai

import com.simiacryptus.openai.FuzzyStringFixer.PartialMatch
import org.apache.commons.text.similarity.LevenshteinDistance

import scala.collection.mutable.ArrayBuffer

object FuzzyStringFixer {
  case class PartialMatch
  (
    adjustedStart: Int,
    rawStart: Int,
    substring: String
  ) {
    require(adjustedStart >= 0)
    require(rawStart >= 0)
    require(substring.nonEmpty)
  }

  /**
   * Finds all partial matches of a pattern in some data, where a partial match is defined as
   * a substring of the data that is at least a certain length and is equal to a substring of
   * the pattern, starting at some index.
   *
   * @param pattern   the pattern to match against
   * @param data      the data to search
   * @param minLength the minimum length of a partial match
   * @param parallel  whether or not to search in parallel
   * @return a list of all partial matches found
   */
  def findPartialMatches(pattern: String, data: String, minLength: Int, parallel: Boolean): List[PartialMatch] = {
    val range = if (parallel) (0 until data.length).par else (0 until data.length)
    range.flatMap(dataIndex => {
      (0 until pattern.length).flatMap(patternIndex => {
        var matchLength = 0
        val maxLength = Math.min(data.length - dataIndex, pattern.length - patternIndex)
        while (matchLength < maxLength && data.charAt(dataIndex + matchLength) == pattern.charAt(patternIndex + matchLength)) {
          matchLength = matchLength + 1
        }
        if (matchLength >= minLength) {
          List(PartialMatch(
            adjustedStart = Math.max(dataIndex - patternIndex, 0),
            rawStart = dataIndex,
            substring = pattern.substring(patternIndex, patternIndex + matchLength)
          ))
        } else {
          List.empty
        }
      })
    }).toArray.toList
  }

}

case class FuzzyStringFixer
(
  pattern: String,
  maxDistance: Int,
  avoid: List[String] = List.empty
) {

  require(pattern.nonEmpty)
  require(maxDistance > 0)
  require(avoid.filter(_.isEmpty).isEmpty)


  case class FuzzyMatch
  (
    start: Int,
    end: Int,
    distance: Int
  )

  lazy val _levenshteinDistance = new LevenshteinDistance(maxDistance + 1)

  def levenshteinDistance(a: String, b: String): Int = {
    val result = _levenshteinDistance.apply(a, b)
    if (result < 0) Integer.MAX_VALUE else result
  }

  /**
   * Replaces all substrings in the given data that match the given pattern within a certain
   * distance with the pattern.
   *
   * @param data the data to search through
   * @param pattern the pattern to look for
   * @param maxDistance the maximum distance that the pattern can be from the data for a match
   * @param avoid a list of patterns to avoid replacing
   * @return the data with the replacements made
   */
  def apply(data: String): String = {
    if (null == data || data.isEmpty) "" else {
      val matches: List[PartialMatch] = FuzzyStringFixer.findPartialMatches(pattern, data, 2, true).filter(_.substring != pattern)
      val filtered = new ArrayBuffer[FuzzyMatch]()
      val sortedMatches = matches.sortBy(_.adjustedStart).toArray.toBuffer
      while (sortedMatches.nonEmpty) {
        val first = sortedMatches.remove(0)
        if (filtered.isEmpty || first.adjustedStart > filtered.map(_.end).max) {
          val fuzzyMatch = refineFullMatch(data, first)
          if (fuzzyMatch.distance <= maxDistance) {
            if (fuzzyMatch.distance > 0) {
              if (avoid.filter(_.nonEmpty).par.filter(refineFullMatch(data, first, _).distance < fuzzyMatch.distance).isEmpty) {
                filtered ++= List(fuzzyMatch)
              }
            }
          }
        }
      }
      var result = data
      var editPos = data.length
      filtered.sortBy(-_.end).foreach(m => {
        if (m.end <= editPos) {
          val lineNumber = if (m.start > 0) data.substring(0, m.start).split('\n').size else 0
          println(s"Replacing ${result.substring(m.start, m.end)} with $pattern, distance ${m.distance} at index ${m.start} (line $lineNumber)")
          result = result.substring(0, m.start) + pattern + result.substring(m.end, result.length)
          editPos = m.start
        }
      })
      result
    }
  }

  /**
   * Refines a partial match by finding the best match for the given pattern within the data.
   *
   * @param data the data to search
   * @param first the partial match to refine
   * @param pattern the pattern to match
   * @return the best match for the given pattern
   */
  def refineFullMatch(data: String, first: PartialMatch, pattern: String = pattern) = {
    require(pattern.nonEmpty)
    var firstIndex = Math.max(first.adjustedStart, 0)
    var lastIndex = Math.min(first.adjustedStart + pattern.length, data.length)
    var distance = levenshteinDistance(data.substring(firstIndex, lastIndex), pattern)
    if (firstIndex > 0 && distance > levenshteinDistance(data.substring(firstIndex - 1, lastIndex), pattern)) {
      do {
        firstIndex = firstIndex - 1
        distance = levenshteinDistance(data.substring(firstIndex, lastIndex), pattern)
      } while (firstIndex > 0 && distance > levenshteinDistance(data.substring(firstIndex - 1, lastIndex), pattern))
    } else {
      while (firstIndex < lastIndex - 1 && distance > levenshteinDistance(data.substring(firstIndex + 1, lastIndex), pattern)) {
        firstIndex = firstIndex + 1
        distance = levenshteinDistance(data.substring(firstIndex, lastIndex), pattern)
      }
    }
    if (distance > levenshteinDistance(data.substring(firstIndex, lastIndex - 1), pattern)) {
      do {
        lastIndex = lastIndex - 1
        distance = levenshteinDistance(data.substring(firstIndex, lastIndex), pattern)
      } while (firstIndex < lastIndex - 1 && distance > levenshteinDistance(data.substring(firstIndex, lastIndex - 1), pattern))
    } else {
      while (lastIndex < data.length - 1 && distance > levenshteinDistance(data.substring(firstIndex, lastIndex + 1), pattern)) {
        lastIndex = lastIndex + 1
        distance = levenshteinDistance(data.substring(firstIndex, lastIndex), pattern)
      }
    }
    FuzzyMatch(firstIndex, lastIndex, distance)
  }


}