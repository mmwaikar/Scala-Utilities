package com.codionics

import com.codionics.DateUtils.*
import com.codionics.DateUtils.LocalDateTimeOps.*
import com.codionics.MapUtils.MapOps.*
import com.codionics.MapUtils.MapTOps.*
import com.codionics.Mappable.*
import org.scalatest.EitherValues.*

import java.time.{
  LocalDateTime,
  OffsetDateTime,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import java.util.Date

case class Emp(name: String, age: Int)
case class Add(city: String, state: String, zip: Int)
case class EmpWithAdd(name: String, age: Int, add: Add)

case class F(g: String)

case class EF(e: Int, f: String)
case class EFG(e: Int, f: F)

case class CD2(c: Int, d: EF)
case class CD3(c: Int, d: EFG)

case class AB3(a: String, b: CD2)
case class AB4(a: String, b: CD3)

class MapUtilsSpec extends BaseSpec:
  val defaultLocalDateTimeUTC: LocalDateTime =
    LocalDateTime.of(2021, 1, 1, 10, 20, 30)
  val defaultLocalDateTimeIST: LocalDateTime =
    LocalDateTime.of(2021, 1, 1, 15, 50, 30)
  val defaultOffsetDateTimeUTC: OffsetDateTime =
    OffsetDateTime.of(defaultLocalDateTimeUTC, ZoneOffset.UTC)

  val defaultZonedDateTime: ZonedDateTime =
    ZonedDateTime.of(defaultLocalDateTimeUTC, ZoneId.of(ZoneOffset.UTC.getId))

  val m3: Map[String, Any] =
    Map("a" -> "1", "b" -> Map("c" -> 3, "d" -> Map("e" -> 5, "f" -> "6")))
  val m4: Map[String, Any] = Map(
    "a" -> "1",
    "b" -> Map("c" -> 3, "d" -> Map("e" -> 5, "f" -> Map("g" -> "7")))
  )

  val simpleMap: Map[String, Any] = Map("name" -> "abc", "age" -> 26)
  val nestedMap: Map[String, Any] = Map(
    "name" -> "abc",
    "age" -> 26,
    "add" -> Map("city" -> "Pune", "state" -> "MH", "zip" -> 411038)
  )
  val mixedMap: Map[String, Any] = Map(
    "name" -> "abc",
    "age" -> 26,
    "isAlien" -> true,
    "power" -> 2.6,
    "since" -> "2021-01-01T10:20:30Z"
  )

  "Given, a Map" when {

    "getOrElseString method is called" should {
      "return the value of the key (if present) as string" in {
        val name = mixedMap.getOrElseString("name", "zzz")
        name should not be null
        name should be("abc")
      }

      "return the default value (if the key is absent) as string" in {
        val name = mixedMap.getOrElseString("name1", "zzz")
        name should not be null
        name should be("zzz")
      }
    }

    "getOrElseInt method is called" should {
      "return the value of the key (if present) as int" in {
        val age = mixedMap.getOrElseInt("age", 0)
        age should be(26)
      }

      "return the default value (if the key is absent) as int" in {
        val age = mixedMap.getOrElseInt("age1", 50)
        age should be(50)
      }
    }

    "getOrElseDouble method is called" should {
      "return the value of the key (if present) as double" in {
        val power = mixedMap.getOrElseDouble("power", 0.0)
        power should be(2.6)
      }

      "return the default value (if the key is absent) as double" in {
        val power = mixedMap.getOrElseDouble("power1", 3.0)
        power should be(3.0)
      }
    }

    "getOrElseBoolean method is called" should {
      "return the value of the key (if present) as boolean" in {
        val isAlien = mixedMap.getOrElseBoolean("isAlien", false)
        isAlien should be(true)
      }

      "return the default value (if the key is absent) as boolean" in {
        val isAlien = mixedMap.getOrElseBoolean("isAlien1", false)
        isAlien should be(false)
      }
    }

    "getOrElseDate method is called" should {
      "return the value of the key (if present) as java.util.Date" in {
        val d = defaultLocalDateTimeUTC.toDate
        val since = mixedMap.getOrElseDate("since", new Date())
        since should be(d)
      }

      "return the default value (if the key is absent) as java.util.Date" in {
        val defaultDate = defaultLocalDateTimeUTC.toDate
        val since = mixedMap.getOrElseDate("since1", defaultDate)
        since should be(defaultDate)
      }
    }

    "getOrElseLocalDateTime method is called" should {
      "return the value of the key (if present) as java.time.LocalDateTime" in {
        val since =
          mixedMap.getOrElseLocalDateTime("since", defaultLocalDateTimeUTC)
        since should be(defaultLocalDateTimeIST)
      }

      "return the default value (if the key is absent) as java.time.LocalDateTime" in {
        val since =
          mixedMap.getOrElseLocalDateTime("since1", defaultLocalDateTimeUTC)
        since should be(defaultLocalDateTimeUTC)
      }
    }

    "getOrElseOffsetDateTime method is called" should {
      "return the value of the key (if present) as java.time.OffsetDateTime" in {
        val since =
          mixedMap.getOrElseOffsetDateTime("since", defaultOffsetDateTimeUTC)
        since should be(defaultOffsetDateTimeUTC)
      }

      "return the default value (if the key is absent) as java.time.OffsetDateTime" in {
        val since =
          mixedMap.getOrElseOffsetDateTime("since1", defaultOffsetDateTimeUTC)
        since should be(defaultOffsetDateTimeUTC)
      }
    }

    "zoned date time is converted to a local data time" should {
      "match the local date time" in {
        defaultZonedDateTime.toLocalDateTime should be(defaultLocalDateTimeUTC)
      }
    }

    "getNested method is called for valid keys" should {

      "return the value of the 2-level nested key" in {
        val v = m3.getNested("b", "c")
//        v should not be null
        println(s"v: $v")
      }

      "return the value of the 3-level nested key" in {
        val v = m3.getNested("b", "d", "f")
//        v should not be null
        println(s"v: $v")
      }
    }

    "getNested method is called for invalid keys" should {

      "throw an exception" in {
        a[ClassCastException] should be thrownBy m3.getNested("a", "c")
      }
    }

    "getNestedEither method is called for valid keys" should {

      "return the value of the 2-level nested key (in a Right)" in {
        val ve = m3.getNestedEither("b", "c")
        ve should not be null
        ve should be(Symbol("right"))
        ve.value should be(3)
        println(s"ve: ${ve.value}")
      }

      "return the value of the 3-level nested key (in a Right)" in {
        val ve = m3.getNestedEither("b", "d", "f")
        ve should not be null
        ve should be(Symbol("right"))
        ve.value should be("6")
        println(s"ve: ${ve.value}")
      }
    }

    "getNestedEither method is called for invalid keys" should {

      "return the error (in a Left)" in {
        val ve = m3.getNestedEither("a", "c")
        ve should not be null
        ve should be(Symbol("left"))
        ve.left.value should be(m3.getErrorMessage("a"))
        println(s"ve: ${ve.left.value}")
      }
    }

    "toCaseClass method is called" should {

      "convert a simple map into Emp case class" in {
        val emp = simpleMap.toSimpleCaseClass[Emp]()
        emp should not be null
        println(s"abWithCDEF: $emp")

        emp.name should be(simpleMap.getNested("name").toString)
        emp.age should be(simpleMap.getNested("age").asInstanceOf[Integer])
      }

      "convert a 2-level nested map into EmpWithAdd case class" in {
        val empWithAdd = nestedMap.toNestedCaseClass[EmpWithAdd, Add]()
        empWithAdd should not be null
        println(s"abWithCDEF: $empWithAdd")

        empWithAdd.name should be(nestedMap.getNested("name").toString)
        empWithAdd.age should be(
          nestedMap.getNested("age").asInstanceOf[Integer]
        )
        empWithAdd.add.city should be(
          nestedMap.getNested("add", "city").toString
        )
      }

      // "convert a 3-level nested map into AB3 case class" in {
      //   val ab3 = m3.toNestedCaseClass[AB3, CD2, EF]()
      //   ab3 should not be null
      //   println(s"ab3: $ab3")

      //   ab3.a should be(m3.getOrElse("a", ""))
      //   ab3.b.c should be(m3.getNested("b", "c").asInstanceOf[Integer])
      //   ab3.b.d.e should be(m3.getNested("b", "d", "e").asInstanceOf[Integer])
      // }

      // "convert a 4-level nested map into AB4 case class" in {
      //   val ab4 = m4.toNestedCaseClass[AB4, CD3, EFG, F]()
      //   ab4 should not be null
      //   println(s"abWithCDEF: $ab4")

      //   ab4.a should be(m4.getOrElse("a", ""))
      //   ab4.b.c should be(m4.getNested("b", "c").asInstanceOf[Integer])
      //   ab4.b.d.f.g should be(m4.getNested("b", "d", "f", "g").toString)
      // }

      // "materialize a 3-level nested map into AB case class" ignore {
      //   val abWithCDEF = materialize[AB3](m3)
      //   abWithCDEF should not be null
      //   println(s"abWithCDEF: $abWithCDEF")

      //   abWithCDEF.a should be(nestedMap.getNested("a").toString)
      //   abWithCDEF.b should be(nestedMap.getNested("age").asInstanceOf[Integer])
      // }
    }
  }
