package com.simiacryptus.openai

/**
 * This is the PersonInfo case class.
 *
 * @param firstName  The person's first name
 * @param middleName The person's middle name
 * @param lastName   The person's last name
 * @param titles     A list of the person's titles
 * @param email      A list of the person's email addresses
 * @param avoid      A list of words to avoid when fixing the person's name
 */
case class PersonInfo
(
  firstName: String,
  middleName: String,
  lastName: String,
  titles: List[String] = List.empty,
  email: List[String] = List.empty,
  avoid: List[String] = List()
) {
  def getFixers() = {
    var nameVariants = List(
      FuzzyStringFixer(s"$lastName", 1, avoid),
      FuzzyStringFixer(s"$firstName", 1, avoid),
      FuzzyStringFixer(s"$firstName $lastName", 3, avoid)
    ).toBuffer
    if (middleName.nonEmpty) nameVariants ++= List(
      FuzzyStringFixer(s"$firstName $middleInitial $lastName", 3, avoid),
      FuzzyStringFixer(s"$firstName $middleInitial. $lastName", 3, avoid),
      FuzzyStringFixer(s"$firstName $middleName $lastName", 3, avoid),
    )
    nameVariants ++= titles.map(title => FuzzyStringFixer(s"$title $lastName", 1, avoid))
    nameVariants = nameVariants.map(x => x.copy(pattern = x.pattern.trim)).filter(_.pattern.length > 4)
    nameVariants ++= email.map(FuzzyStringFixer(_, 3, avoid))
    nameVariants.distinct
  }

  def middleInitial = if (middleName.isEmpty) "" else middleName.substring(0, 1)
}
