package nl.jqno.fpscala.ch9_parsers

import language.higherKinds

sealed trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val comma = (spaces ** char(',') ** spaces) map (t => t._1._1 + t._1._2 + t._2)

    val jnull: Parser[JNull.type] =
      string("null") map (_ => JNull)
    val jnumber: Parser[JNumber] =
      regex("\\d+.\\d+".r) map (n => JNumber(n.toDouble))
    val jstring: Parser[JString] =
      for {
        _ <- char('"')
        s <- regex("[^\"]*".r)
        _ <- char('"')
      } yield JString(s)
    val jbool: Parser[JBool] =
      regex("true|false".r) map (b => if (b == "true") JBool(true) else JBool(false))

    val jarrayEmpty: Parser[JArray] =
      succeed(JArray(IndexedSeq.empty[JSON]))
    val jarrayBase: Parser[JArray] =
      for {
        e <- jsonParser(P)
      } yield JArray(IndexedSeq(e))
    lazy val jarrayInduction: Parser[JArray] =
      (for {
        es <- jarrayInduction
        _ <- spaces
        _ <- char(',')
        _ <- spaces
        e <- jarrayBase
      } yield JArray(es.get :+ e)) | jarrayBase
    val jarray: Parser[JArray] =
      for {
        _ <- char('[')
        _ <- spaces
        as <- jarrayInduction | jarrayEmpty
        _ <- spaces
        _ <- char(']')
      } yield as

    val jobjectEmpty: Parser[JObject] =
      succeed(JObject(Map.empty))
    val jobjectBase: Parser[JObject] =
      for {
        k <- jstring
        _ <- spaces
        _ <- char(':')
        _ <- spaces
        v <- jsonParser(P)
      } yield JObject(Map(k.get -> v))
    lazy val jobjectInduction: Parser[JObject] =
      (for {
        _ <- spaces
        e <- jobjectBase
        _ <- spaces
        _ <- char(',')
        es <- jobjectInduction
      } yield JObject(es.get ++ e.get)) | succeed(JObject(Map.empty))
    val jobject: Parser[JObject] =
      for {
        _ <- char('{')
        _ <- spaces
        es <- jobjectInduction | jobjectEmpty
        _ <- spaces
        _ <- char('}')
      } yield es

    jobject
  }
}
