package com.simiacryptus.openai

import org.apache.commons.text.similarity.LevenshteinDistance

import scala.collection.mutable.ArrayBuffer

trait Wrangler {

  def findClusters(strings: Seq[String], threshold: Int) = {
    findClusters_raw(
      candidates = strings
        .filter(_.size > 8)
        .filter(_.size < 64)
        .filter(!_.toUpperCase.contains("EXHIBIT"))
        .filter(!_.toUpperCase.contains("$")),
      threshold = threshold)
  }

  def findClusters_raw(candidates: Seq[String], threshold: Int) = {
    def clusterSeeds(subjects: Seq[String], objects: Seq[String]) = {
      println(s"Finding clusters of radius ${threshold} among ${subjects.size} x ${objects.size} strings")
      subjects.par.map(l => l -> {
        val distance = new LevenshteinDistance(threshold)
        objects.filter(l2 => distance(l, l2) != -1).toArray
      }).toArray.map(t => (List(t._1) ++ t._2).toArray)
    }

    val clusters0: Array[Array[String]] = clusterSeeds(candidates.toList, candidates.toList)
    //    val clusters0: Array[Array[String]] = candidates.keys.flatMap(size=>{
    //      clusterSeeds(candidates(size), (for (r <- ((size - 2) until (size + 2))) yield {
    //        candidates.get(r).toList.flatten
    //      }).flatten.toList)
    //    }).toArray

    val clusters1 = new ArrayBuffer[Array[String]]()
    if (false) {
      println(s"Aggregating ${clusters0.size} clusters (${clusters0.map(_.size).sum} lines)")
      while (clusters0.flatten.filter(!clusters1.flatten.contains(_)).nonEmpty) {
        val usedLines = clusters1.flatten.toSet
        val newCluster = clusters0.toList.sortBy(-_.par.count(!usedLines.contains(_))).head
        clusters1 += newCluster
      }
    } else {
      clusters1 ++= clusters0
    }

    println(s"Found ${clusters1.size} clusters")
    clusters1.toArray
  }

}
