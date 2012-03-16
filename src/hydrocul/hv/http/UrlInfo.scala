package hydrocul.hv.http;

/**
 * URLをパースした結果を保持するクラス。
 * URLの文字列に戻したときに正確に元のURLを再現できるように
 * パラメータの順序などの必要な情報を保持している。
 * アンカーリンク("#")には対応していない。
 */
case class UrlInfo (
  scheme: String,
  usernameAndPassword: Option[(String, String)],
  host: String,
  port: Option[Int],
  path: String, // "/" から始まる文字列。ドメインまたはポート番号で終わるURLの場合は、空文字列。
  query: Option[Seq[(String, Option[String])]]
){

  def urlWithAuth: String = getUrlSub(true);

  def url: String = getUrlSub(false);

  private def getUrlSub(includingAuth: Boolean): String = {
    getSchemeAuthHostPort(includingAuth) +
    path +
    query.map("?" + UrlInfo.queryToUrlEncoded(_)).getOrElse("");
  }

  private def getSchemeAuthHostPort(includingAuth: Boolean): String = {
    scheme + "://" +
    (if(!includingAuth || !usernameAndPassword.isDefined){
      "";
    } else {
      val u = usernameAndPassword.get;
      u._1 + ":" + u._2 + "@";
    }) +
    host +
    (if(!port.isDefined){
      "";
    } else {
      ":" + port.get;
    });
  }

  def requestPath: String = {
    (if(path.isEmpty) "/" else path) +
    query.map("?" + UrlInfo.queryToUrlEncoded(_)).getOrElse("");
  }

  /**
   * GETパラメータを追加したUrlInfoを返す。
   * すでに存在するキーがあれば、上書きする。
   */
  def addQueryParams(param: Map[String, String]): UrlInfo = {
    if(param.isEmpty)
      return this;
    val newQuery = param.keySet.toSeq.sorted.foldLeft(query.getOrElse(Nil)){ (seq, key) =>
      val value = param(key);
      if(seq.map(_._1).contains(key)){
        // すでに存在するキーを上書き
        seq.filter(_._1 != key) :+ (key, Some(value));
      } else {
        // キーを追加
        seq :+ (key, Some(value));
      }
    }
    UrlInfo(scheme, usernameAndPassword, host, port, path, Some(newQuery));
  }

  def createUrl(relativePath: String): Option[UrlInfo] =
    createUrlSub(relativePath, true);

  private def createUrlSub(relativePath: String, first: Boolean): Option[UrlInfo] = {
    if(first){
      val p = relativePath.indexOf('#');
      if(p >= 0){
        return createUrlSub(relativePath.substring(0, p), first);
      }
    }
    val relativePathI = relativePath.toLowerCase;
    if(first && relativePathI.startsWith("mailto:")){
      None;
    } else if(first && relativePathI.startsWith("javascript:")){
      None;
    } else if(first && relativePathI.startsWith("tel:")){
      None;
    } else if(first && relativePathI.startsWith("http://")){
      Some(UrlInfo(relativePath));
    } else if(first && relativePathI.startsWith("https://")){
      Some(UrlInfo(relativePath));
    } else if(first && relativePath.startsWith("//")){
      Some(UrlInfo(scheme + ":" + relativePath));
    } else if(first && relativePath.startsWith("/")){
      Some(UrlInfo(getSchemeAuthHostPort(true) + relativePath));
    } else if(relativePath.isEmpty){
      Some(this);
    } else if(relativePath.equals(".")){
      Some(dirUrl);
    } else if(relativePath.equals("..")){
      parentDirUrl match {
        case None => Some(this);
        case Some(u) => Some(u);
      }
    } else if(relativePath.startsWith("./")){
      dirUrl.createUrlSub(relativePath.substring(2), false);
    } else if(relativePath.startsWith("../")){
      parentDirUrl match {
        case None => createUrlSub(relativePath.substring(3), false);
        case Some(u) => u.createUrlSub(relativePath.substring(3), false);
      }
    } else {
      val p = relativePath.indexOf('/');
      if(p < 0){
        Some(UrlInfo(urlWithAuth + relativePath));
      } else {
        UrlInfo(urlWithAuth + relativePath.substring(0, p + 1)).
          createUrlSub(relativePath.substring(p + 1), false);
      }
    }
  }

  private def dirUrl: UrlInfo = {
    if(path.isEmpty){
      this;
    } else {
      val p = path.lastIndexOf('/');
      UrlInfo(scheme, usernameAndPassword, host, port, path.substring(0, p + 1), query);
    }
  }

  private def parentDirUrl: Option[UrlInfo] = {
    if(path.isEmpty){
      None;
    } else {
      val p = path.lastIndexOf('/');
      val p2 = path.substring(0, p).lastIndexOf('/');
      if(p2 < 0){
        None;
      } else {
        Some(UrlInfo(scheme, usernameAndPassword, host, port, path.substring(0, p + 1), query));
      }
    }
  }

}

object UrlInfo {

  private val Pattern1 = "(https?)://([^/]+)".r;
  private val Pattern2 = "(https?)://([^/]+)(/.*)".r;

  def apply(url: String): UrlInfo = {
    url match {
      case UrlPattern11(scheme, host) =>
        new UrlInfo(scheme, None, host, None, "", None);
      case UrlPattern12(scheme, host, path) =>
        new UrlInfo(scheme, None, host, None, path, None);
      case UrlPattern13(scheme, host, path, query) =>
        new UrlInfo(scheme, None, host, None, path, Some(parseQuery(query)));
      // TODO ポート番号・認証情報に未対応
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

  /**
   * a=1&b=2 の形式の文字列にする。引数に渡すパラメータはURLエンコード済みの前提とする。
   */
  private[http] def queryToUrlEncoded(query: Seq[(String, Option[String])]): String = {
    query.map { p =>
      if(!p._2.isDefined){
        p._1;
      } else {
        p._1 + "=" + p._2.get;
      }
    }.mkString("&");
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
    { // test UrlInfo.apply, UrlInfo#url
      def sub(expected: UrlInfo, url: String): Seq[Option[String]] = {
        val actual = apply(url);
        List(
          assertEquals(expected, actual),
          assertEquals(url, actual.url)
        );
      }
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "", None),
        "http://www.yahoo.co.jp") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/", None),
        "http://www.yahoo.co.jp/") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("1"))))),
        "http://www.yahoo.co.jp/abc?A=1") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("1")), ("B", Some("2"))))),
        "http://www.yahoo.co.jp/abc?A=1&B=2") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", None)))),
        "http://www.yahoo.co.jp/abc?A") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some(""))))),
        "http://www.yahoo.co.jp/abc?A=")
    } ++
    { // test UrlInfo#addQueryParams
      List(
        assertEquals(
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some("")), ("B", Some("2"))))),
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some(""))))).addQueryParams(Map("B" -> "2"))
        ),
        assertEquals(
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some("2"))))),
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some(""))))).addQueryParams(Map("A" -> "2"))
        )
      );
    }
  }

  def defaultPort(scheme: String): Int = {
    scheme match {
      case "http" => 80;
      case "https" => 443;
      case _ => throw new IllegalArgumentException;
    }
  }

}
