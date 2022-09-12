package com.codionics

import com.codionics.IntOps.*
import scala.util.Try
import java.util.UUID
import java.net.{URI, URL, URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import com.codionics.StringUtils.StringOps.toUUIDOption

object StringUtils:
  val vowels: Seq[String] = Seq("a", "e", "i", "o", "u")

  object StringOps:
    extension (s: String)
      def isNullOrEmpty: Boolean = (s == null) || s.trim.isEmpty

      def isNotNullOrEmpty: Boolean = !isNullOrEmpty

      def isInt: Boolean =
        Try(s.toInt).toOption match
          case Some(_) => true
          case None    => false

      def toStringOption: Option[String] =
        if (s.isNullOrEmpty) None else Some(s)

      def toUUIDOption: Option[UUID] = Try(UUID.fromString(s)).toOption

      def toBigDecimal: BigDecimal = Try(BigDecimal(s)).getOrElse(BigDecimal(0))

      def queryStringToMap: Map[String, String] = {
        val pairs = s.split("[&=]").grouped(2)

        /** If the query string is malformed and there is no "=" sign between
          * any of the two terms, then some of the pairs might contain only one
          * element, so retain only valid pairs.
          */
        val validPairs = pairs.filter(_.length.isEven)
        validPairs.map { case Array(k, v) => k -> v }.toMap
      }

      /** Removes all tab characters (\t), and leading and trailing whitespace.
        * @return
        *   A string with any tab characters, leading and trailing whitespace
        *   removed.
        */
      def removeTabs(): String = s.replaceAll("\t", "").trim

      /** Removes all linefeed characters (\n), and leading and trailing
        * whitespace.
        * @return
        *   A string with any linefeed characters, leading and trailing
        *   whitespace removed.
        */
      def removeLineFeeds(): String = s.replaceAll("\n", "").trim

      /** Removes all carriage-return / linefeed characters (\r\n), and leading
        * and trailing whitespace.
        * @return
        *   A string with any carriage-return / linefeed characters, leading and
        *   trailing whitespace removed.
        */
      def removeCarriageReturnsLineFeeds(): String =
        s.replaceAll("\r\n", "").trim

      /** Removes all tab characters (\t), linefeed characters (\n),
        * carriage-return / linefeed characters (\r\n), and leading and trailing
        * whitespace.
        * @return
        *   A string with any tab, linefeed, carriage-return / linefeed
        *   characters, leading and trailing whitespace removed.
        */
      def removeAllWhitespace(): String =
        s.removeTabs().removeLineFeeds().removeCarriageReturnsLineFeeds()

      /** Finds the string to the right of the match string
        * @param matchString
        *   the string to find in the original string
        * @return
        *   the original string if the match string is not present in the
        *   original string, else the string to the right of the match string
        */
      def toRightOf(matchString: String): String = {
        val index = s.indexOf(matchString)

        if (index == -1) s
        else {
          val length = s.length - (index + matchString.length)
          if (isInvalidStringLength(length)) s else s.takeRight(length)
        }
      }

      def between(excludingStart: String, excludingEnd: String): String = {
        val length = s.length
        val startIndex = s.indexOf(excludingStart)
        val endIndex = s.indexOf(excludingEnd)

        val invalidStartEndIndexTuple =
          (startIndex < 0, endIndex < 0 || endIndex > length)
        invalidStartEndIndexTuple match {
          case (true, true) => s
          case (true, _)    => s.substring(0, endIndex)
          case (_, true)    => s.substring(startIndex + 1, length)
          case (_, _)       => s.substring(startIndex + 1, endIndex)
        }
      }

      /** Finds if a word starts with a vowel.
        * @param word
        *   the word
        * @return
        *   true if the word starts with a vowel, else false
        */
      def startsWithVowel(word: String): Boolean =
        if (word.isNullOrEmpty) false
        else {
          val firstChar = word.toLowerCase.head.toString
          vowels.contains(firstChar)
        }

      def isInvalidStringLength(length: Int): Boolean =
        length < 0 || length > s.length

      def urlEncodeUtf8: String =
        URLEncoder.encode(s, StandardCharsets.UTF_8.name())

      def urlDecodeUtf8: String =
        URLDecoder.decode(s, StandardCharsets.UTF_8.name())

      def canConvertToInt: Boolean = Try(Integer.parseInt(s)).isSuccess

      def toIntOption: Option[Int] = Try(Integer.parseInt(s)).toOption

      def toIntNoThrow(default: Int): Int =
        Try(Integer.parseInt(s)).toOption.getOrElse(default)

      // returns a 32-character MD5 hash version of the input string
      def md5Hash: String = {
        import java.math.BigInteger
        import java.security.MessageDigest
        val md = MessageDigest.getInstance("MD5")
        val digest: Array[Byte] = md.digest(s.getBytes)
        val bigInt = new BigInteger(1, digest)
        val hashedString = bigInt.toString(16).trim
        prependWithZeros(hashedString)
      }

      /** This uses a little magic in that the string I start with is a “format
        * specifier,” and it states that the string it returns should be
        * prepended with blank spaces as needed to make the string length equal
        * to 32. Then I replace those blank spaces with the character `0`.
        */
      private def prependWithZeros(inputString: String): String =
        "%1$32s".format(inputString).replace(' ', '0')

  object OptionStringOps:
    extension (s: Option[String])
      def isNullOrEmpty: Boolean =
        (s == null) || s.isEmpty || s.get.trim.isEmpty

      def isNotNullOrEmpty: Boolean = !isNullOrEmpty

      def toUUID: Option[UUID] = s.flatMap(_.toUUIDOption)

      def toIntOption: Option[Int] =
        s.flatMap(x => Try(Integer.parseInt(x)).toOption)

      def toIntNoThrow(default: Int): Int =
        s.flatMap(x => Try(Integer.parseInt(x)).toOption).getOrElse(default)
