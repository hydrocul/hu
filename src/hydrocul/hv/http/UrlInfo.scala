package hydrocul.hv.http;

/**
 * URLをパースした結果を保持するクラス。
 * URLの文字列に戻したときに正確に元のURLを再現できるように
 * パラメータの順序などの必要な情報を保持している。
 */
private[http] case class UrlInfo (
  scheme: String,
  usernameAndPassword: Option[(String, String)],
  host: String,
  port: Option[Int],
  path: String, // "/" から始まる文字列。ドメインまたはポート番号で終わるURLの場合は、空文字列。
  query: Option[Seq[(String, Option[String])]]
){

  def url: String = {
    scheme + "://" +
    (if(!usernameAndPassword.isDefined){
      "";
    } else {
      val u = usernameAndPassword.get;
      u._1 + ":" + u._2;
    }) +
    host +
    (if(!port.isDefined){
      "";
    } else {
      ":" + port.get;
    }) +
    path +
    queryStr;
  }

  def requestPath: String = {
    path +
    queryStr;
  }

  private def queryStr: String = {
    if(!query.isDefined){
      "";
    } else {
      query.get.map { p =>
        if(!p._2.isDefined){
          p._1;
        } else {
          p._1 + "=" + p._2.get;
        }
      }.mkString("?", "&", "");
    }
  }

}

private[http] object UrlInfo {

  def apply(url: String): UrlInfo = {
    url match {
      case UrlPattern11(scheme, host) =>
        new UrlInfo(scheme, None, host, None, "", None);
      case UrlPattern12(scheme, host, path) =>
        new UrlInfo(scheme, None, host, None, path, None);
      case UrlPattern13(scheme, host, path, query) =>
        new UrlInfo(scheme, None, host, None, path, Some(parseQuery(query)));
      // TODO
      case _ =>
        throw new IllegalArgumentException(url);
    }
    
  }

  /**
   * URLの "?" より後の文字列をパースする。URLデコードはしない。
   */
  private def parseQuery(query: String): Seq[(String, Option[String])] = {
    query.split("&").map { s =>
      val p = s.split("=", 2);
      if(p.size < 2){
        (p(0), None);
      } else {
        (p(0), Some(p(1)));
      }
    }
  }

  // TODO 認証情報に未対応
  private val UrlPattern11 = ("(https?)://([^:/]+)").r;
  private val UrlPattern12 = ("(https?)://([^:/]+)" +     "(/[^?]*)").r;
  private val UrlPattern13 = ("(https?)://([^:/]+)" +     "(/[^?]*)\\?(.*)").r;
  private val UrlPattern21 = ("(https?)://([^:/]+)(:[0-9]+)").r;
  private val UrlPattern22 = ("(https?)://([^:/]+)(:[0-9]+)(/[^?]*)").r;
  private val UrlPattern23 = ("(https?)://([^:/]+)(:[0-9]+)(/[^?]*)\\?(.*)").r;

  private[http] def test(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;
    List(
      assertEquals(UrlInfo("http", None, "www.yahoo.co.jp", None, "", None),
        apply("http://www.yahoo.co.jp")),
      assertEquals(UrlInfo("http", None, "www.yahoo.co.jp", None, "/", None),
        apply("http://www.yahoo.co.jp/")),
      assertEquals(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("1"))))),
        apply("http://www.yahoo.co.jp/abc?A=1")),
      assertEquals(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("1")), ("B", Some("2"))))),
        apply("http://www.yahoo.co.jp/abc?A=1&B=2")),
      assertEquals(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", None)))),
        apply("http://www.yahoo.co.jp/abc?A")),
      assertEquals(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some(""))))),
        apply("http://www.yahoo.co.jp/abc?A="))
    );
  }

}
