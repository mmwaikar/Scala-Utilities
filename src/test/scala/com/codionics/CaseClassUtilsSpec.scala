package com.codionics

import com.codionics.CaseClassUtils.*

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
  }
}
