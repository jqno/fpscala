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


  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
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
    lazy val innerJarray: Parser[JArray] = // incorrect: still allows trailing comma's. But I don't care yet ;)
      (for {
        j <- jsonParser(P)
        _ <- comma
        js <- innerJarray
      } yield JArray(js.get :+ j)) | succeed(JArray(IndexedSeq.empty[JSON]))
    val jarray: Parser[JArray] =
      for {
        _ <- char('[')
        _ <- spaces
        a <- innerJarray
        _ <- spaces
        _ <- char(']')
      } yield a
    val entryJobject: Parser[(String, JSON)] =
      for {
        k <- jstring
        _ <- spaces
        _ <- char('=')
        _ <- spaces
        v <- jsonParser(P)
      } yield (k.get, v)
    lazy val entriesJobject: Parser[JObject] =
      (for {
        _ <- spaces
        e <- entryJobject
        _ <- spaces
        _ <- char(',')
        es <- entriesJobject
      } yield JObject(es.get + e)) | succeed(JObject(Map.empty))
    val jobject: Parser[JObject] =
      for {
        _ <- char('{')
        es <- entriesJobject
        _ <- spaces
        _ <- char('}')
      } yield es


    jobject
  }
}
