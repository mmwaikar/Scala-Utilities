package com.codionics

import com.codionics.CaseClassUtils.*

case class CCOptionalFields(name: String, age: Option[Int] = None)

class CaseClassUtilsSpec extends BaseSpec {

  "Given CaseClassUtils" when {

    "toMap method is called on a nested case class" should {
      "return a map with the inner case class being returned as a nested map" in {
        val ewa = EmpWithAdd("E1", 50, Add("C1", "S1", 411038))
        val tm = ewa.toMap
        tm should not be null
        println(s"ewa to map: $tm")
      }
    }

    "toMap method is called on a 3-level nested case class" should {
      "return a map with nested case classes as nested maps" in {
        val cd3 = CD3(1, EFG(2, F("g")))
        val tm = cd3.toMap
        tm should not be null
        println(s"cd3 to map: $tm")
      }
    }

    "toMap method is called on a 4-level nested case class" should {
      "return a map with nested case classes as nested maps" in {
        val cd3 = CD3(1, EFG(2, F("g")))
        val ab4 = AB4("a", cd3)
        val tm = ab4.toMap
        tm should not be null
        println(s"ab4 to map: $tm")
      }
    }

    "toMap method is called for a case class with None optional values" should {
      "return a map with empty map for None optional values" in {
        val cc = CCOptionalFields("FL")
        val m = cc.toMap
        m should not be null
        println(s"cc to map with option: $m")
      }
    }

    "toMapWithNoKeysForEmptyOptionalValues method is called" should {
      "return a map with no keys for None optional values" in {
        val cc = CCOptionalFields("FL")
        val m = cc.toMapWithNoKeysForEmptyOptionalValues
        m should not be null
        println(s"cc to map with no key for None optional values: $m")
        m.contains("age") should be(false)
      }

      "return a map with keys for Some optional values" in {
        val cc = CCOptionalFields("FL", Some(10))
        val m = cc.toMapWithNoKeysForEmptyOptionalValues
        m should not be null
        println(s"cc to map with key for Some optional values: $m")
        m.contains("age") should be(true)
      }
    }
  }
}
