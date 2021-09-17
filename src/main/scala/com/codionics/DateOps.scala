package com.codionics

import java.util.Date
import java.time.{LocalDateTime, ZoneId, ZoneOffset}

object DateOps:
  extension (date: Date)
    def toLocalDateTime: LocalDateTime =
      date.toInstant.atZone(ZoneId.of(ZoneOffset.UTC.getId)).toLocalDateTime
