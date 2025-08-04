package esmeta.web.http

import cats.effect.*
import io.circe.*, io.circe.syntax.*
import org.http4s.MediaType
import org.http4s.dsl.io.*
import org.http4s.headers.`Content-Type`

extension [T: Encoder](t: T)
  def asJsonOk = Ok(t.asJson.noSpaces)
    .map(_.withContentType(`Content-Type`(MediaType.application.json)))
