package esmeta.tracer
package util

import io.circe.*
import io.circe.generic.semiauto.*

object JsonProtocol {
  import Trace.*

  given Decoder[Trace.Binary] = deriveDecoder
  given Decoder[Trace.BinaryPre] = deriveDecoder
  given Decoder[Trace.Conditional] = deriveDecoder
  given Decoder[Trace.Declare] = deriveDecoder
  given Decoder[Trace.ForInObject] = deriveDecoder
  given Decoder[Trace.FunctionEnter] = deriveDecoder
  given Decoder[Trace.FunctionExit] = deriveDecoder
  given Decoder[Trace.GetField] = deriveDecoder
  given Decoder[Trace.GetFieldPre] = deriveDecoder
  given Decoder[Trace.InvokeFun] = deriveDecoder
  given Decoder[Trace.InvokeFunPre] = deriveDecoder
  given Decoder[Trace.Literal] = deriveDecoder
  given Decoder[Trace.PutField] = deriveDecoder
  given Decoder[Trace.PutFieldPre] = deriveDecoder
  given Decoder[Trace.Read] = deriveDecoder
  given Decoder[Trace.Unary] = deriveDecoder
  given Decoder[Trace.UnaryPre] = deriveDecoder
  given Decoder[Trace.Write] = deriveDecoder
  given Decoder[Trace.Print] = deriveDecoder

  given Decoder[Trace] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "binary"        => cursor.as[Trace.Binary]
      case "binaryPre"     => cursor.as[Trace.BinaryPre]
      case "conditional"   => cursor.as[Trace.Conditional]
      case "declare"       => cursor.as[Trace.Declare]
      case "forinObject"   => cursor.as[Trace.ForInObject]
      case "functionEnter" => cursor.as[Trace.FunctionEnter]
      case "functionExit"  => cursor.as[Trace.FunctionExit]
      case "getField"      => cursor.as[Trace.GetField]
      case "getFieldPre"   => cursor.as[Trace.GetFieldPre]
      case "invokeFun"     => cursor.as[Trace.InvokeFun]
      case "invokeFunPre"  => cursor.as[Trace.InvokeFunPre]
      case "literal"       => cursor.as[Trace.Literal]
      case "putField"      => cursor.as[Trace.PutField]
      case "putFieldPre"   => cursor.as[Trace.PutFieldPre]
      case "read"          => cursor.as[Trace.Read]
      case "unary"         => cursor.as[Trace.Unary]
      case "unaryPre"      => cursor.as[Trace.UnaryPre]
      case "write"         => cursor.as[Trace.Write]
      case "print"         => cursor.as[Trace.Print]
      case other =>
        Left(DecodingFailure(s"Unknown type: $other", cursor.history))
    }
  }

  // others
  given Encoder[CachedTrace] = deriveEncoder
  given Decoder[CachedTrace] = deriveDecoder
  given Encoder[TargetKind] = deriveEncoder
  given Decoder[TargetKind] = deriveDecoder
}
