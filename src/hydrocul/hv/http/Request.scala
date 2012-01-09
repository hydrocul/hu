package hydrocul.hv.http;

import hydrocul.hv.EncodingMania;

private[http] object Request {

  def createGet(url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): Array[Byte] =
    EncodingMania.encodeChar(
      createGetSub(url, cookie, requestHeader),
      "ISO-8859-1");

  def createPost(url: UrlInfo, postParam: Map[String, String],
      cookie: Map[String, String], requestHeader: Seq[(String, String)]): Array[Byte] =
    EncodingMania.encodeChar(
      createPostSub(url, postParam, cookie, requestHeader),
      "ISO-8859-1");

  val defaultHeader: Seq[(String, String)] = List(
    ("User-Agent", "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)") // IE7
  );

  private def createGetSub(url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): String = {
    "GET " + url.requestPath + " HTTP/1.1\r\n" +
    createHeaderLines(url, requestHeader, cookie) +
    "\r\n";
  }

  private def createPostSub(url: UrlInfo, postParam: Map[String, String],
      cookie: Map[String, String], requestHeader: Seq[(String, String)]): String = {
    throw new Exception("// TODO");
  }

  private def createHeaderLines(url: UrlInfo, requestHeader: Seq[(String, String)],
      cookie: Map[String, String]): String = {
    val hostLine = requestHeader.find(_._1 == "Host") match {
      case Some(_) => "";
      case None => "Host: " + url.host + "\r\n";
    }
    hostLine +
    requestHeader.map { kv =>
      kv._1 + ": " + kv._2 + "\r\n";
    }.mkString;
    // TODO cookieの処理
  }

}
