package com.codionics

import com.github.sisyphsu.dateparser.DateParserUtils

import java.time.{LocalDateTime, OffsetDateTime}
import java.util.Date
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Try

object MapUtils:
  object MapOps:
    extension (map: Map[String, Any])
      def getOrElseString(key: String, defaultValue: String = ""): String =
        map.getOrElse(key, defaultValue).toString

      def getOrElseInt(key: String, defaultValue: Int = 0): Int =
        map.getOrElse(key, defaultValue).toString.toInt

      def getOrElseDouble(key: String, defaultValue: Double = 0.0): Double =
        map.getOrElse(key, defaultValue).toString.toDouble

      def getOrElseBoolean(
          key: String,
          defaultValue: Boolean = false
      ): Boolean =
        map.getOrElse(key, defaultValue).toString.toBoolean

      /** Gets the value of the map key as a java.util.Date. If the key is not
        * found, or if the string cannot be parsed as a date, then return the
        * defaultValue.
        * @param key
        *   key in the map
        * @param defaultValue
        *   default value if the key is not found
        * @return
        *   the map key as a java.util.Date. If the key is not found, or if the
        *   string cannot be parsed as a date, then return the defaultValue
        */
      def getOrElseDate(key: String, defaultValue: Date = new Date()): Date =
        val dateStr = map.getOrElse(key, defaultValue).toString
        Try(DateParserUtils.parseDate(dateStr)).getOrElse(defaultValue)

      /** Gets the value of the map key as a java.time.LocalDateTime. If the key
        * is not found, or if the string cannot be parsed as a date, then return
        * the defaultValue.
        * @param key
        *   key in the map
        * @param defaultValue
        *   default value if the key is not found
        * @return
        *   the map key as a java.time.LocalDateTime. If the key is not found,
        *   or if the string cannot be parsed as a date, then return the
        *   defaultValue
        */
      def getOrElseLocalDateTime(
          key: String,
          defaultValue: LocalDateTime = LocalDateTime.now()
      ): LocalDateTime =
        val dateStr = map.getOrElse(key, defaultValue).toString
        DateParserUtils.parseDateTime(dateStr)

      /** Gets the value of the map key as a java.time.OffsetDateTime. If the
        * key is not found, or if the string cannot be parsed as a date, then
        * return the defaultValue.
        * @param key
        *   key in the map
        * @param defaultValue
        *   default value if the key is not found
        * @return
        *   the map key as a java.time.OffsetDateTime. If the key is not found,
        *   or if the string cannot be parsed as a date, then return the
        *   defaultValue
        */
      def getOrElseOffsetDateTime(
          key: String,
          defaultValue: OffsetDateTime = OffsetDateTime.now()
      ): OffsetDateTime =
        val dateStr = map.getOrElse(key, defaultValue).toString
        DateParserUtils.parseOffsetDateTime(dateStr)

      /** Returns the value in a nested associative structure, where keys is a
        * variable number of keys.
        * @param keys
        *   variable number of keys
        * @return
        *   the value of the nested key (if found), else a blank string ""
        */
      def getNested(keys: String*): Any =
        getNested(keys, map)

      /** Returns a Right(value) in a nested associative structure, where keys
        * is a variable number of keys.
        * @param keys
        *   variable number of keys
        * @return
        *   a Right(value) of the nested key (if found), else a Right("") or a
        *   Left(error) in case of an exception
        */
      def getNestedEither(keys: String*): Either[String, Any] =
        getNestedEither(keys, map)

      def getErrorMessage(key: String) =
        s"The value of the key: $key is not a map."

      @tailrec
      private def getNested(
          keys: Seq[String],
          m: Map[String, Any] = Map.empty
      ): Any = {
        if (keys.size == 1) {
          val key = keys.head

          if (m.contains(key)) {
            m(key)
          } else {
            ""
          }
        } else {
          val key = keys.head
          val inner = m.getOrElse(key, Map.empty).asInstanceOf[Map[String, Any]]
          getNested(keys.tail, inner)
        }
      }

      private def getNestedEither(
          keys: Seq[String],
          m: Map[String, Any] = Map.empty
      ): Either[String, Any] =
        try {
          Right(getNested(keys, m))
        } catch {
          case _: ClassCastException => Left(getErrorMessage(keys.head))
          case t: Throwable          => Left(t.toString)
        }

  object MapTOps:
    extension (m: Map[String, Any])
      def toSimpleCaseClass[T]()(implicit classTag: ClassTag[T]): T = {
        val ctor = classTag.runtimeClass.getConstructors.head
        val args = classTag.runtimeClass.getDeclaredFields.map(f =>
          m(f.getName).asInstanceOf[AnyRef]
        )
        ctor.newInstance(args: _*).asInstanceOf[T]
      }

      def toNestedCaseClass[T, U]()(implicit
          classTag: ClassTag[T],
          classTag1: ClassTag[U]
      ): T = {
        val ctor = classTag.runtimeClass.getConstructors.head
        val types = ctor.getGenericParameterTypes
        val args = classTag.runtimeClass.getDeclaredFields.map(f =>
          m(f.getName).asInstanceOf[AnyRef]
        )
        val typeArgTuples = types.zip(args)

        val newArgs = typeArgTuples.map { case (_, a) =>
          a match {
            case value: Map[_, _] =>
              val inner =
                value.asInstanceOf[Map[String, Any]].toSimpleCaseClass[U]()
              inner.asInstanceOf[AnyRef]
            case _ => a
          }
        }

        ctor.newInstance(newArgs: _*).asInstanceOf[T]
      }

      // def toNestedCaseClass[T, U, V]()(implicit
      //     classTag: ClassTag[T],
      //     classTag1: ClassTag[U],
      //     classTag2: ClassTag[V]
      // ): T = {
      //   val ctor = classTag.runtimeClass.getConstructors.head
      //   val types = ctor.getGenericParameterTypes
      //   val args = classTag.runtimeClass.getDeclaredFields.map(f =>
      //     m(f.getName).asInstanceOf[AnyRef]
      //   )
      //   val typeArgTuples = types.zip(args)

      //   val newArgs = typeArgTuples.map { case (_, a) =>
      //     a match {
      //       case value: Map[U, V] =>
      //         val inner =
      //           value.asInstanceOf[Map[String, Any]].toNestedCaseClass[U, V]()
      //         inner.asInstanceOf[AnyRef]
      //       case _ => a
      //     }
      //   }

      //   ctor.newInstance(newArgs: _*).asInstanceOf[T]
      // }

      // def toNestedCaseClass[T, U, V, W]()(implicit
      //     classTag: ClassTag[T],
      //     classTag1: ClassTag[U],
      //     classTag2: ClassTag[V],
      //     classTag3: ClassTag[W]
      // ): T = {
      //   val ctor = classTag.runtimeClass.getConstructors.head
      //   val types = ctor.getGenericParameterTypes
      //   val args = classTag.runtimeClass.getDeclaredFields.map(f =>
      //     m(f.getName).asInstanceOf[AnyRef]
      //   )
      //   val typeArgTuples = types.zip(args)

      //   val newArgs = typeArgTuples.map { case (_, a) =>
      //     a match {
      //       case value: Map[_, _] =>
      //         val inner =
      //           value.asInstanceOf[Map[String, Any]].toNestedCaseClass[U, V, W]
      //         inner.asInstanceOf[AnyRef]
      //       case _ => a
      //     }
      //   }

      //   ctor.newInstance(newArgs: _*).asInstanceOf[T]
      // }

      private def toCaseClass[T]()(implicit classTag: ClassTag[T]): T = {
        val ctor = classTag.runtimeClass.getConstructors.head
        val args = classTag.runtimeClass.getDeclaredFields.map(f =>
          m(f.getName).asInstanceOf[AnyRef]
        )
        ctor.newInstance(args: _*).asInstanceOf[T]
      }
