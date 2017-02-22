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

    val jnull: Parser[JSON] =
      string("null") map (_ => JNull)
    val jnumber: Parser[JSON] =
      regex("\\d+.\\d+".r) map (n => JNumber(n.toDouble))
    val jstring: Parser[JSON] =
      for {
        _ <- char('"')
        s <- regex("[^\"]*".r)
        _ <- char('"')
      } yield JString(s)
    val jbool: Parser[JSON] =
      regex("true|false".r) map (b => if (b == "true") JBool(true) else JBool(false))
    lazy val innerJarray: Parser[JArray] = // incorrect: still allows trailing comma's. But I don't care yet ;)
      (for {
        j <- jsonParser(P)
        _ <- comma
        js <- innerJarray
      } yield JArray(js.get :+ j)) | succeed(JArray(IndexedSeq.empty[JSON]))
    val jarray: Parser[JSON] =
      for {
        _ <- char('[')
        _ <- spaces
        a <- innerJarray
        _ <- spaces
        _ <- char(']')
      } yield a


    ???
  }
}
