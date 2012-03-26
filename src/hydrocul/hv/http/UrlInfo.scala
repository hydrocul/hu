package hydrocul.hv.http;

import hydrocul.hv.EncodingMania;

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
  query: Option[Seq[(String, Option[String])]], // クエリの文字列はURLエンコード済み
  anchor: Option[String]
){

  def urlWithAuth: String = getUrlSub(true);

  def url: String = getUrlSub(false);

  private def getUrlSub(includingAuth: Boolean): String = {
    getSchemeAuthHostPort(includingAuth) +
    path +
    query.map("?" + UrlInfo.queryToUrlEncoded(_)).getOrElse("") +
    anchor.map("#" + _).getOrElse("");
  }

  private def getSchemeAuthHostPort(includingAuth: Boolean): String = {
    scheme + "://" +
    (if(!includingAuth || !usernameAndPassword.isDefined){
      "";
    } else {
      val u = usernameAndPassword.get;
      u._1 + ":" + u._2 + "@";
    }) +
    hostAndPort;
  }

  def hostAndPort: String = {
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
    UrlInfo(scheme, usernameAndPassword, host, port, path, Some(newQuery), anchor);
  }

  def updateAnchor(anchor: Option[String]): UrlInfo = {
    UrlInfo(scheme, usernameAndPassword, host, port, path, query, anchor);
  }

  def createUrl(relativePath: String): Option[UrlInfo] =
    createUrlSub(relativePath, true);

  private def createUrlSub(relativePath: String, first: Boolean): Option[UrlInfo] = {
    if(first){
      val p = relativePath.indexOf('#');
      if(p >= 0){
        return createUrlSub(relativePath.substring(0, p), first).
          map(_.updateAnchor(Some(relativePath.substring(p + 1))));
      }
    }
    {
      val p = relativePath.indexOf('#');
      if(p >= 0){
        throw new IllegalArgumentException(relativePath);
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
        Some(UrlInfo(dirUrl.urlWithAuth + relativePath));
      } else {
        UrlInfo(dirUrl.urlWithAuth + relativePath.substring(0, p + 1)).
          createUrlSub(relativePath.substring(p + 1), false);
      }
    }
  }

  private def dirUrl: UrlInfo = {
    if(path.isEmpty){
      UrlInfo(scheme, usernameAndPassword, host, port, "/", None, None);
    } else {
      // path は空文字列か "/" で始まる文字列
      val p = path.lastIndexOf('/');
      UrlInfo(scheme, usernameAndPassword, host, port, path.substring(0, p + 1), None, None);
    }
  }

  private def parentDirUrl: Option[UrlInfo] = {
    if(path.isEmpty){
      None;
    } else {
      // path は空文字列か "/" で始まる文字列
      val p = path.lastIndexOf('/');
      val p2 = path.substring(0, p).lastIndexOf('/');
      if(p2 < 0){
        None;
      } else {
        Some(UrlInfo(scheme, usernameAndPassword, host, port, path.substring(0, p2 + 1), None, None));
      }
    }
  }

}

object UrlInfo {

  def apply(url: String): UrlInfo = {
    val (scheme, authHostPort, pathQuery, anchor) = url match {
      case UrlPattern1(scheme, authHostPort, pathQuery) =>
        (scheme, authHostPort, pathQuery, None);
      case UrlPattern2(scheme, authHostPort, pathQuery, anchor) =>
        (scheme, authHostPort, pathQuery, Some(anchor));
      case _ =>
        throw new IllegalArgumentException(url);
    }
    val (usernameAndPassword, host, port) = authHostPort match {
      case AuthHostPortPattern1(host) =>
        (None, host, None);
      case AuthHostPortPattern2(host, port) =>
        (None, host, Some(port.toInt));
      case AuthHostPortPattern3(id, pw, host) =>
        (Some((id, pw)), host, None);
      case AuthHostPortPattern4(id, pw, host, port) =>
        (Some((id, pw)), host, Some(port.toInt));
    }
    val (path, query) = pathQuery match {
      case PathQueryPattern1() =>
        ("", None);
      case PathQueryPattern2(path) =>
        (path, None);
      case PathQueryPattern3(query) =>
        ("", Some(parseQuery(query)));
      case PathQueryPattern4(path, query) =>
        (path, Some(parseQuery(query)));
    }
    new UrlInfo(scheme, usernameAndPassword, host, port, path, query, anchor);
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
   * "a=1&b=2" の形式の文字列にする。
   * 引数に渡すパラメータはURLエンコード済みの前提とする。
   */
  private[http] def queryToUrlEncoded(query: Seq[(String, Option[String])]): String = {
    queryToUrlEncoded(query, None);
  }

  /**
   * "a=1&b=2" の形式の文字列にする。encoding=None の場合は
   * 引数に渡すパラメータはURLエンコード済みの前提とする。
   */
  private[http] def queryToUrlEncoded(query: Seq[(String, Option[String])],
      encoding: Option[String]): String = {
    query.map { p =>
      val p2 = if(encoding.isDefined){
        (
          EncodingMania.encodeUrl(p._1, encoding.get),
          p._2.map(EncodingMania.encodeUrl(_, encoding.get))
        );
      } else {
        p;
      }
      if(!p2._2.isDefined){
        p2._1;
      } else {
        p2._1 + "=" + p._2.get;
      }
    }.mkString("&");
  }

  /**
   * "a=1; b=2;" の形式の文字列にする。引数に渡すパラメータはURLエンコード済みの前提とする。
   */
  private[http] def cookieToUrlEncoded(query: Seq[(String, Option[String])],
      encoding: Option[String]): String = {
    query.map { p =>
      val p2 = if(encoding.isDefined){
        (
          EncodingMania.encodeUrl(p._1, encoding.get),
          p._2.map(EncodingMania.encodeUrl(_, encoding.get))
        );
      } else {
        p;
      }
      if(!p2._2.isDefined){
        p2._1;
      } else {
        p2._1 + "=" + p._2.get;
      }
    }.mkString("", "; ", ";");
  }

  private val UrlPattern1 = ("(https?)://([^/]+)([^#]*)").r;
  private val UrlPattern2 = ("(https?)://([^/]+)([^#]*)#(.*)").r;

  private val AuthHostPortPattern1 = "([^:/]+)".r;
  private val AuthHostPortPattern2 = "([^:/]+):([1-9][0-9]*)".r;
  private val AuthHostPortPattern3 = "([^:@]+):([^:@]+)@([^:/]+)".r;
  private val AuthHostPortPattern4 = "([^:@]+):([^:@]+)@([^:/]+):([1-9][0-9]*)".r;

  private val PathQueryPattern1 = "".r;
  private val PathQueryPattern2 = "(/[^?#]*)".r;
  private val PathQueryPattern3 = "\\?([^#]*)".r;
  private val PathQueryPattern4 = "(/[^?#]*)\\?([^#]*)".r;

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
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "", None, None),
        "http://www.yahoo.co.jp") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/", None, None),
        "http://www.yahoo.co.jp/") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("1")))), None),
        "http://www.yahoo.co.jp/abc?A=1") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("1")), ("B", Some("2")))), None),
        "http://www.yahoo.co.jp/abc?A=1&B=2") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", None))), None),
        "http://www.yahoo.co.jp/abc?A") ++
      sub(UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
        Some(List(("A", Some("")))), None),
        "http://www.yahoo.co.jp/abc?A=")
    } ++
    { // test UrlInfo#dirUrl
      List(
        assertEquals(
          UrlInfo("http://www.yahoo.co.jp/"),
          UrlInfo("http://www.yahoo.co.jp").dirUrl
        ),
        assertEquals(
          UrlInfo("http://www.yahoo.co.jp/"),
          UrlInfo("http://www.yahoo.co.jp/").dirUrl
        ),
        assertEquals(
          UrlInfo("http://www.yahoo.co.jp/"),
          UrlInfo("http://www.yahoo.co.jp/abc").dirUrl
        ),
        assertEquals(
          UrlInfo("http://www.yahoo.co.jp/abc/"),
          UrlInfo("http://www.yahoo.co.jp/abc/").dirUrl
        ),
        assertEquals(
          UrlInfo("http://www.yahoo.co.jp/abc/"),
          UrlInfo("http://www.yahoo.co.jp/abc/def").dirUrl
        )
      );
    } ++
    { // test UrlInfo#parentDirUrl
      List(
        assertEquals(
          None,
          UrlInfo("http://www.yahoo.co.jp").parentDirUrl
        ),
        assertEquals(
          None,
          UrlInfo("http://www.yahoo.co.jp/").parentDirUrl
        ),
        assertEquals(
          None,
          UrlInfo("http://www.yahoo.co.jp/abc").parentDirUrl
        ),
        assertEquals(
          Some(UrlInfo("http://www.yahoo.co.jp/")),
          UrlInfo("http://www.yahoo.co.jp/abc/").parentDirUrl
        ),
        assertEquals(
          Some(UrlInfo("http://www.yahoo.co.jp/")),
          UrlInfo("http://www.yahoo.co.jp/abc/def").parentDirUrl
        )
      );
    } ++
    { // test UrlInfo#addQueryParams
      List(
        assertEquals(
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some("")), ("B", Some("2")))), None),
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some("")))), None).addQueryParams(Map("B" -> "2"))
        ),
        assertEquals(
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some("2")))), None),
          UrlInfo("http", None, "www.yahoo.co.jp", None, "/abc",
            Some(List(("A", Some("")))), None).addQueryParams(Map("A" -> "2"))
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
