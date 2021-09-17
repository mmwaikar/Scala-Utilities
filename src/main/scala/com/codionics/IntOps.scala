package com.codionics

object IntOps:
  extension (n: Int)
    def isEven: Boolean = n % 2 == 0

    def isOdd: Boolean = !isEven