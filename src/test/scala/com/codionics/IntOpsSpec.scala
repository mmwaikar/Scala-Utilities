package com.codionics

import com.codionics.IntOps._
import org.scalacheck.Gen

class IntOpsSpec extends BaseSpec {

  val evenInts: Gen[Int] = for (n <- Gen.choose(-1000, 1000)) yield 2 * n
  val oddInts: Gen[Int] = evenInts.map(_ + 1)

  "Given IntOps" when {

    "isEven method is called" should {

      "return true for even numbers" in {
        forAll(evenInts) { n =>
          n.isEven should be(true)
        }
      }

      "return false for odd numbers" in {
        forAll(oddInts) { n =>
          n.isEven should be(false)
        }
      }
    }

    "isOdd method is called" should {

      "return true for odd numbers" in {
        forAll(oddInts) { n =>
          n.isOdd should be(true)
        }
      }

      "return false for even numbers" in {
        forAll(evenInts) { n =>
          n.isOdd should be(false)
        }
      }
    }
  }
}
