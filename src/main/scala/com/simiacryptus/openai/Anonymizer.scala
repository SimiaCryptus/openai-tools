package com.simiacryptus.openai

import java.util.regex.Pattern
import scala.util.Random.shuffle

object Anonymizer {
  val firstNames: List[String] = "Michael\nChristopher\nJessica\nMatthew\nAshley\nJennifer\nJoshua\nAmanda\nDaniel\nDavid\nJames\nRobert\nJohn\nJoseph\nAndrew\nRyan\nBrandon\nJason\nJustin\nSarah\nWilliam\nJonathan\nStephanie\nBrian\nNicole\nNicholas\nAnthony\nHeather\nEric\nElizabeth\nAdam\nMegan\nMelissa\nKevin\nSteven\nThomas\nTimothy\nChristina\nKyle\nRachel\nLaura\nLauren\nAmber\nBrittany\nDanielle\nRichard\nKimberly\nJeffrey\nAmy\nCrystal\nMichelle\nTiffany\nJeremy\nBenjamin\nMark\nEmily\nAaron\nCharles\nRebecca\nJacob\nStephen\nPatrick\nSean\nErin\nZachary\nJamie\nKelly\nSamantha\nNathan\nSara\nDustin\nPaul\nAngela\nTyler\nScott\nKatherine\nAndrea\nGregory\nErica\nMary\nTravis\nLisa\nKenneth\nBryan\nLindsey\nKristen\nJose\nAlexander\nJesse\nKatie\nLindsay\nShannon\nVanessa\nCourtney\nChristine\nAlicia\nCody\nAllison\nBradley\nSamuel\nShawn\nApril\nDerek\nKathryn\nKristin\nChad\nJenna\nTara\nMaria\nKrystal\nJared\nAnna\nEdward\nJulie\nPeter\nHolly\nMarcus\nKristina\nNatalie\nJordan\nVictoria\nJacqueline\nCorey\nKeith\nMonica\nJuan\nDonald\nCassandra\nMeghan\nJoel\nShane\nPhillip\nPatricia\nBrett\nRonald\nCatherine\nGeorge\nAntonio\nCynthia\nStacy\nKathleen\nRaymond\nCarlos\nBrandi\nDouglas\nNathaniel\nIan\nCraig\nBrandy\nAlex\nValerie\nVeronica\nCory\nWhitney\nGary\nDerrick\nPhilip\nLuis\nDiana\nChelsea\nLeslie\nCaitlin\nLeah\nNatasha\nErika\nCasey\nLatoya\nErik\nDana\nVictor\nBrent\nDominique\nFrank\nBrittney\nEvan\nGabriel\nJulia\nCandice\nKaren\nMelanie\nAdrian\nStacey\nMargaret\nSheena\nWesley\nVincent\nAlexandra\nKatrina\nBethany\nNichole\nLarry\nJeffery\nCurtis\nCarrie\nTodd\nBlake\nChristian\nRandy\nDennis\nAlison\nTrevor\nSeth\nKara\nJoanna\nRachael\nLuke\nFelicia\nBrooke\nAustin\nCandace\nJasmine\nJesus\nAlan\nSusan\nSandra\nTracy\nKayla\nNancy\nTina\nKrystle\nRussell\nJeremiah\nCarl\nMiguel\nTony\nAlexis\nGina\nJillian\nPamela\nMitchell\nHannah\nRenee\nDenise\nMolly\nJerry\nMisty\nMario\nJohnathan\nJaclyn\nBrenda\nTerry\nLacey\nShaun\nDevin\nHeidi\nTroy\nLucas\nDesiree\nJorge".split('\n').toList.map(_.trim)
  val lastNames: List[String] = "Adams\nAnderson\nArlington\nArmstrong\nBaker\nBaldwin\nBearden \nBenjamin \nBernardinetti \nBerryman \nBiddle \nBlumenthal \nBoozman \nBradley \nBrady \nBriant \nBrooks \nBrown \nBurr\nButterfield \nCallahan \nCamden \nCampbell \nCarlson\nCarney \nCarrillo \nCarter \nCasarez-Otero \nCastro \nChabot\nChancellor \nChapman \nCharleston \nCheney\nCherryholmes \nChilds\nClay\nCleveland \nClinton \nColbert \nConaway \nCooksey".split('\n').toList.map(_.trim)
}

case class Anonymizer(people: Seq[PersonInfo], data: List[String]) {
  val firstNames: List[String] = Anonymizer.firstNames.filter(w=>data.find(_.toLowerCase.contains(w.toLowerCase)).isEmpty).map(_.trim.toLowerCase).distinct.toArray.toList
  val lastNames: List[String] = Anonymizer.lastNames.filter(w=>data.find(_.toLowerCase.contains(w.toLowerCase)).isEmpty).map(_.trim.toLowerCase).distinct.toArray.toList

  val censorMap = {
    val censoredLastNames = people.map(_.lastName.toLowerCase).distinct.sorted.toArray
    val censoredFirstNames = people.map(_.firstName.toLowerCase)
      .filter(!censoredLastNames.contains(_)).distinct.sorted.toArray
    censoredLastNames.zip(shuffle(lastNames.take(censoredLastNames.size))).toMap ++
      censoredFirstNames.zip(shuffle(firstNames.take(censoredLastNames.size))).toMap
  }
  val reverseMap = censorMap.map(t=>t._2->t._1)

  def deanonymize(data: String): String = {
    reverseMap.foldLeft(data)((data, t) => data.replaceAll("(?i)"+Pattern.quote(t._1),t._2))
  }

  def anonymize(data: String): String = {
    censorMap.foldLeft(data)((data, t) => data.replaceAll("(?i)"+Pattern.quote(t._1),t._2))
  }

  def editLines(fn: String => String) = (data:String) => {
    var safeData: String = anonymize(data)
    if (deanonymize(safeData).toLowerCase != data.toLowerCase) {
      throw new RuntimeException(s"""Confidential: $data\nLookback: $safeData\n""")
    }
    safeData = (for (line <- safeData.split('\n')) yield fn(line)).mkString("\n").trim
    deanonymize(safeData)
  }

  def edit(fn: String => String) = (data:String) => {
    var safeData = anonymize(data)
    if (deanonymize(safeData).toLowerCase != data.toLowerCase) {
      throw new RuntimeException(s"""Confidential: $data\nLookback: $safeData\n""")
    }
    safeData = fn(safeData)
    deanonymize(safeData)
  }

}
