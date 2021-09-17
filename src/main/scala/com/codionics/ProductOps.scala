package com.codionics

object ProductOps:
  extension (p: scala.Product)
    def toNestedMap[A](a: A): Map[String, Any] = 
      val tuples = toNameValueTuples(a)
      tuples.map { case (k, v) =>
        if (v.isInstanceOf[Product]) (k, toNestedMap(v))
        else (k, v)
      }.toMap
    
    def toSimpleMap[A](a: A): Map[String, Any] = 
      toNameValueTuples(a).toMap
    
    def toNameValueTuples[A](a: A): Iterator[(String, Any)] = 
      if (a.isInstanceOf[Product]) {
        val p: Product = a.asInstanceOf[Product]
        val names = p.productElementNames
        val values = p.productIterator
        names.zip(values)
      } else {
        Iterator.empty[(String, Any)]
      }
