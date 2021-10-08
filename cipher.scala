import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex
import java.io.File
import java.io.PrintWriter

// I had help from Stack Overflow, GeeksforGeeks, Dustin, Marcus, and Liam

object cipher extends App {

  // prepareText reads text from a file and returns a string with only the letters
  def prepareText(file: String): String = {
    // read text from a file into a string
    val fileText = Source.fromFile(file).mkString
    // create a regular expression that matches upper and lower case letters
    val rgx: Regex = "[a-zA-Z]".r
    // apply the regular expression
    val strippedText = (rgx findAllIn fileText).mkString.toLowerCase
    // note: in scala, the "return" keyword is not necessary
    // return strippedText
    strippedText
  }

  // strip removes instances of duplicate characters in a string and returns a list of unique characters
  // "j"s are also replaced with "i"s
  def strip(str: String): List[Char] = {
    val removeJ = str.replace("j", "i")
    val listStr = removeJ.toList
    // the distinct method returns a list without any duplicates
    val lst = listStr.distinct
    // return lst
    lst
  }

  // createPairs creates and returns an array of pairs of strings
  // duplicates that would appear in pairs are separated by "x"
  def createPairs(str: String): ArrayBuffer[String] = {
    // newString will hold a string with x's separating duplicate characters that would appear in a pair
    var newString = ""
    // removeJ is a string where "j"s are replaced with "i"s
    val removeJ = str.replace("j", "i")
    // charArray is an array of characters from the given string
    val charArray = removeJ.toCharArray
    // pairs will hold strings, each of which will contain two characters
    val pairs = ArrayBuffer[String]()

    // use range to access every other item in charArray
    for (char <- Range(0, charArray.size - 1, 2)) {
      // if two items that would appear in a pair are the same, add both items to newString with an "x" between them
      if (charArray(char) == charArray(char + 1)) {
        // note: Scala Arrays do not allow element insertion
        newString += charArray(char).toString + "x" + charArray(char + 1).toString
      // if the items are not the same, just add both to newString
      } else {
        newString += charArray(char).toString + charArray(char + 1).toString
      }
    }
    // if charArray has an odd number of characters, add the last element to the new string
    if (charArray.size % 2 != 0) {
      newString += charArray(charArray.size - 1)
    }

    // for every other element in newString, add the current and next element to a pair, and add the pair to pairs
    // note: in Scala, length is used for strings instead of size
    for (char <- Range(0, newString.length - 1, 2)) {
      pairs += newString(char).toString + newString(char + 1).toString
    }
    // if newString's length is odd, create a pair with the last element and "x" and add it to pairs
    if (newString.length % 2 != 0) {
      pairs += newString(newString.length - 1).toString + "x"
    }
    // return pairs
    pairs
  }

  // square creates an array of arrays to represent the key square
  def square(str: String): Array[Array[String]] = {
    // sq is an array of 5 arrays
    val sq = Array.ofDim[String](5, 5)
    // strippedString is the given string but without duplicate characters
    val strippedString = strip(str)
    // alphabet is a list of characters in the alphabet, minus "j"
    val alphabet = "abcdefghiklmnopqrstuvwxyz".toList
    // letters is a combination of strippedString and alphabet but with no duplicate characters
    // note: ::: concatenates two lists in Scala
    val letters = (strippedString ::: alphabet).distinct

    // create an iterator to iterate through character in letters
    val iter = letters.iterator

    // iterate through the rows and columns of the square
    for (i <- Range(0, 5)) {
      for (j <- Range(0, 5)) {
        // assign the iterator value to the current location in the square
        sq(i)(j) = iter.next.toString
      }
    }
    // return the square
    sq
  }

  // findLoc finds the location of a given string in a given key square
  // returns an array containing a row value and column value
  def findLoc(letter: String, sq: Array[Array[String]]): Array[Int] = {

    val returnVal = Array(-1, -1)

    // iterate through the rows and columns of the square
    for (row <- Range(0, 5)) {
      for (column <- Range(0, 5)) {
        // if the current location in the square holds the given string, set the return value to the location
        if (sq(row)(column) == letter) {
          returnVal(0) = row
          returnVal(1) = column
        }
      }
    }
    // return the location
    returnVal
  }

  // encrypt encrypts a given string using a given key square
  def encrypt(plainText: String, sq: Array[Array[String]]): String = {
    // splitText is an ArrayBuffer containing pairs of letters
    val splitText = createPairs(plainText)
    // finalString will hold the final value of the encrypted string
    var finalString = ""

    // iterate through the pairs in splitText
    for (item <- splitText) {
      // get the locations of each letter in the pair
      val loc1 = findLoc(item(0).toString, sq)
      val loc2 = findLoc(item(1).toString, sq)

      // if both letters are in the same column
      if (loc1(1) == loc2(1)) {
        // the new letter is the letter below the current letter
        // if the current letter is at the bottom, the new letter is the letter at the top of the column
        val newLetter1 = sq((loc1(0) + 1) % 5)(loc1(1))
        val newLetter2 = sq((loc2(0) + 1) % 5)(loc2(1))
        // add the new letters to finalString
        finalString += newLetter1 + newLetter2
      }

      // if both letters are in the same row
      else if (loc1(0) == loc2(0)) {

        // the new letter if the letter to the right of the current letter
        // if the current letter is in the rightmost position, the new letter is the letter in the leftmost position
        val newLetter1 = sq(loc1(0))((loc1(1) + 1) % 5)
        val newLetter2 = sq(loc2(0))((loc2(1) + 1) % 5)
        // add the new letters to finalString
        finalString += newLetter1 + newLetter2
      }

      // if the letters in the pair don't share a row or column
      else {
        // a rectangle is formed with the two current letters, and the new letter is the letter diagonal to the current letter
        val newLetter1 = sq(loc1(0))(loc2(1))
        val newLetter2 = sq(loc2(0))(loc1(1))
        // add the new letters to finalString
        finalString += newLetter1 + newLetter2
      }
    }
    // return finalString
    finalString
  }

  // call prepareText on input.txt to get a string to use as input
  val input = prepareText("input.txt")
  println("Input: " + input)

  // create and print the key square
  println("\nKey square:\n")
  val sq = square("computerscience")
  for (arr <- sq) {
    for (item <- arr) {
      print(item + "  ")
    }
    println()
  }

  // encrypt the input using the key square
  val encrypted = encrypt("hello", sq)
  println("\nOutput: " + encrypted)

  // write the encrypted string to an output file and close the file
  val writer = new PrintWriter(new File("output.txt"))
  writer.write(encrypted)
  writer.close()
}
