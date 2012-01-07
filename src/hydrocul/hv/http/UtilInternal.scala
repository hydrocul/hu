package hydrocul.hv.http;

import hydrocul.hv.EncodingMania;

private[http] object UtilInternal {

  def createRequestGet(url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): Array[Byte] =
    EncodingMania.encodeChar(
      createRequestGetSub(url, cookie, requestHeader),
      "ISO-8859-1");

  def createRequestPost(url: UrlInfo, postParam: Map[String, String],
      cookie: Map[String, String], requestHeader: Seq[(String, String)]): Array[Byte] =
    EncodingMania.encodeChar(
      createRequestPostSub(url, postParam, cookie, requestHeader),
      "ISO-8859-1");

  private def createRequestGetSub(url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): String = {
    throw new Exception("// TODO");
  }

  private def createRequestPostSub(url: UrlInfo, postParam: Map[String, String],
      cookie: Map[String, String], requestHeader: Seq[(String, String)]): String = {
    throw new Exception("// TODO");
  }

  val defaultRequestHeader: Seq[(String, String)] = List(
    ("User-Agent", "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)") // IE7
  );

  def parseResponse(response: Array[Byte]): Response = {
    throw new Exception("// TODO");
  }

}
