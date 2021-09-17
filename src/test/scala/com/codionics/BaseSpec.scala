package com.codionics

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class BaseSpec
  extends AnyWordSpec
    with should.Matchers
    with OptionValues
    with ScalaCheckDrivenPropertyChecks {

}