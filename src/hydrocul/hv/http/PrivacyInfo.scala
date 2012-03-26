package hydrocul.hv.http;

@SerialVersionUID(4082634928601961217L)
class PrivacyInfo private (
  val cookies: Seq[Cookie]
){

  /**
   * リクエストヘッダーの文字列を取得する。
   */
  def getCookies(urlInfo: UrlInfo): String = {
    val hostAndPort = urlInfo.hostAndPort;
    val requestPath = urlInfo.requestPath;
    val cookies2 = cookies.filter { c =>
      c.domain == hostAndPort &&
      requestPath.startsWith(c.path)
      // TODO expires と secure を見る必要あり
    }
    Cookie.createRequestHeaderValue(cookies2);
  }

  /**
   * レスポンスヘッダーの Set-Cookie の値を与えて、新しいクッキー情報を取得する。
   */
  def setCookies(urlInfo: UrlInfo, setcookieValues: Seq[String]): PrivacyInfo = {
    setcookieValues.foldLeft(this)(_.setCookie(urlInfo, _));
  }

  private def setCookie(urlInfo: UrlInfo, setcookieValue: String): PrivacyInfo = {
    val newCookie = Cookie.fromResponseHeader(urlInfo, setcookieValue);
    val newCookies = cookies.filterNot { c =>
      c.name == newCookie.name &&
      c.path == newCookie.path &&
      c.domain == newCookie.domain;
    } :+ newCookie;
    new PrivacyInfo(newCookies);
  }

  override def toString(): String = {
    cookies.map(_.toString).mkString("PrivacyInfo\n  ", "\n  ", "");
  }

}

object PrivacyInfo {

  def apply(): PrivacyInfo = {
    new PrivacyInfo(Nil);
  }

}
