package hydrocul.hv.http;

import hydrocul.hv.EncodingMania;

case class Cookie (
  name: String,
  value: String,
  expires: Option[Long],
  path: String, // "/" で始まる文字列
  domain: String,
  secure: Boolean
){
}

object Cookie {

  private[http] def fromResponseHeader(url: UrlInfo, setcookieValue: String): Cookie = {
    val m1: Seq[(String, String)] = setcookieValue.split(";").map { s =>
      val s2 = s.split("=");
      if(s2.size >= 2){
        (s2(0), s2(1).trim);
      } else {
        (s2(0), "");
      }
    };
    val name = EncodingMania.decodeUrl(m1(0)._1, "UTF-8");;
    val value = EncodingMania.decodeUrl(m1(0)._2, "UTF-8");
    val m2: Map[String, String] = m1.drop(1).toMap;
    val expires: Option[Long] = {
      val expires = m2.get("expires");
      // TODO expiresをパースしてLongに変換する必要あり
      None;
    }
    val path: String = {
      val path = m2.get("path").getOrElse(url.path);
      if(path.isEmpty)
        "/";
      else
        path;
    }
    val domain: String = {
      val domain = m2.get("domain").getOrElse(url.hostAndPort);
      // TODO 正キュリティのためにドメインの種類に応じたピリオドの数のチェックが必要
      // 例えば .jp などと指定されてしまうと、第三者のクッキーを受け入れてしまうため
      // url.hostAndPort を適用した場合にピリオドを先頭に付けなくていいのか調査が必要
      domain;
    }
    val secure: Boolean = m2.isDefinedAt("secure");
    Cookie(EncodingMania.decodeUrl(name, "UTF-8"),
      EncodingMania.decodeUrl(value, "UTF-8"),
      expires, path, domain, secure);
  }

  private[http] def createRequestHeaderValue(cookies: Seq[Cookie]): String = {
    UrlInfo.cookieToUrlEncoded(cookies.map(c => (c.name, Some(c.value))), Some("UTF-8"));
  }

  private[http] def test(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;
    List(
      assertEquals(
        Cookie("a", "A", None, "/", "www.yahoo.co.jp", false),
        fromResponseHeader(UrlInfo("http://www.yahoo.co.jp"),
          "a=A; expires=Tue, 26-Mar-2014 20:00:00 GMT; path=/; domain=.yahoo.co.jp")
      )
    );
  }

}
