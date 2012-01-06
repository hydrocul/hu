package hydrocul.hv.http;

private[http] object UtilInternal {

/* TODO
  def createRequestGet(url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): Array[Byte];

  def createRequestPost(url: UrlInfo, postParam: Map[String, String],
      cookie: Map[String, String], requestHeader: Seq[(String, String)]): Array[Byte];
*/

  val defaultRequestHeader: Seq[(String, String)] = List(
    ("User-Agent", "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)") // IE7
  );

}
