package com.codionics

import java.time.{LocalDate, LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.Date

object DateUtils:
  object DateOps:
    extension (date: Date)
      def toLocalDateTime: LocalDateTime =
        date.toInstant.atZone(ZoneId.of(ZoneOffset.UTC.getId)).toLocalDateTime

      def toZonedDateTime: ZonedDateTime = {

        /** NOTE: since in Cassandra, the time zone information goes as +0000,
          * we must use UTC to convert the date back to a ZonedDateTime.
          */
        ZonedDateTime.ofInstant(date.toInstant, ZoneId.of(ZoneOffset.UTC.getId))
      }

  object LocalDateOps:
    extension (localDate: LocalDate)
      def toDate: Date =
        Date.from(
          localDate.atStartOfDay(ZoneId.of(ZoneOffset.UTC.getId)).toInstant
        )

  object LocalDateTimeOps:
    extension (localDateTime: LocalDateTime)
      def toDate: Date =
        Date.from(
          localDateTime.atZone(ZoneId.of(ZoneOffset.UTC.getId)).toInstant
        )

  object ZonedDateTimeOps:
    extension (zonedDateTime: ZonedDateTime)
      def toDate: Date = {

        /** NOTE: a java.util.date does not have time zone information, hence in
          * Cassandra, the time zone information goes as +0000 which corresponds
          * to UTC time zone.
          */
        Date.from(zonedDateTime.toInstant)
      }
