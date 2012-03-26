package hydrocul.hv.http;

import hydrocul.hv.EncodingMania;

@deprecated("do not construct instances", "2012/03/26")
class WebBrowser private () {
  // classは存在意義がないが、過去との互換性のために維持しておく

  def doGet(url: String): Page =
    WebBrowser.doGet(url);

  def doPost(url: String, postParam: Map[String, String]): Page =
    WebBrowser.doPost(url, postParam);

}

object WebBrowser {

  @deprecated("do not construct instances", "2012/03/26")
  def create(): WebBrowser = new WebBrowser(); // 過去との互換性のために維持しておく

  def doGet(url: String): Page = {
    val urlInfo = UrlInfo(url);
    val response = Sockets.doGet(urlInfo.host, urlInfo, Map.empty, createRequestHeaderGet(""));
    responseToPage(url, response);
  }

  def doGet(url: String, privacyInfo: PrivacyInfo):
      (Page, PrivacyInfo) = {
    val urlInfo = UrlInfo(url);
    val cookie = privacyInfo.getCookies(urlInfo);
    val response = Sockets.doGet(urlInfo.host, urlInfo, Map.empty, createRequestHeaderGet(cookie));
    val page = responseToPage(url, response);
    val newPrivacyInfo = modifyPrivacyInfo(privacyInfo, urlInfo, page);
    (page, newPrivacyInfo);
  }

  def doPost(url: String, postParam: Map[String, String]): Page = {
    val urlInfo = UrlInfo(url);
    val postStr = UrlInfo.queryToUrlEncoded(paramMapToSeq(postParam));
    val postBin = EncodingMania.encodeChar(postStr, "ISO-8859-1");
    val response = Sockets.doPost(urlInfo.host, urlInfo, Map.empty,
      createRequestHeaderPost("", postBin.length),
      postBin);
    responseToPage(url, response);
  }

  def doPost(url: String, postParam: Map[String, String], privacyInfo: PrivacyInfo):
      (Page, PrivacyInfo) = {
    val urlInfo = UrlInfo(url);
    val cookie = privacyInfo.getCookies(urlInfo);
    val postStr = UrlInfo.queryToUrlEncoded(paramMapToSeq(postParam));
    val postBin = EncodingMania.encodeChar(postStr, "ISO-8859-1");
    val response = Sockets.doPost(urlInfo.host, urlInfo, Map.empty,
      createRequestHeaderPost(cookie, postBin.length),
      postBin);
    val page = responseToPage(url, response);
    val newPrivacyInfo = modifyPrivacyInfo(privacyInfo, urlInfo, page);
    (page, newPrivacyInfo);
  }

  private def createRequestHeaderGet(cookie: String): Seq[(String, String)] = {
    if(cookie.isEmpty){
      Request.defaultHeader;
    } else {
      Request.defaultHeader :+
      ("Cookie", cookie);
    }
  }

  private def createRequestHeaderPost(cookie: String, postLen: Int): Seq[(String, String)] = {
    val h = if(cookie.isEmpty){
      Request.defaultHeader;
    } else {
      Request.defaultHeader :+
      ("Cookie", cookie);
    }
    h :+
      ("Content-Type" -> "application/x-www-form-urlencoded") :+
      ("Content-Length" -> postLen.toString);
  }

  private[http] def paramMapToSeq(query: Map[String, String]):
    Seq[(String, Option[String])] =
    query.map(t => (t._1, Some(EncodingMania.encodeUrl(t._2, "UTF-8")))).toSeq;

  private def responseToPage(url: String, response: Response): Page = {
    response.contentType match {
      case Some("text/html") =>
        new HtmlPage(response, url);
      case Some("application/json") =>
        new JsonPage(response, url);
      case _ =>
        new BinaryPage(response, url);
    }
  }

  private def modifyPrivacyInfo(privacyInfo: PrivacyInfo,
      urlInfo: UrlInfo, page: Page): PrivacyInfo = {
    // page は将来HTMLを解釈してクッキーをセットするため
    privacyInfo.setCookies(urlInfo, page.response.cookies);
  }

  private[hv] def test(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    import hydrocul.hv.TestLib._;
    testSub(all) ++
    List[(String, Function0[Seq[Option[String]]])](
      ("http.UrlInfo", UrlInfo.test),
      ("http.Cookie", Cookie.test),
      ("http.Response", Response.test)
    );
  }

  private def testSub(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    if(!all){
      return Nil;
    }
    import hydrocul.hv.TestLib._;
    ("http.Webbrowser", { () =>
      val browser = WebBrowser;
      val page1 = browser.doGet("http://www.yahoo.co.jp/");
      val page2 = browser.doGet("https://twitter.com/");
      val page3 = browser.doPost("http://www.ugtop.com/spill.shtml",
        Map("a" -> "AAA"));
      val page4 = browser.doPost("https://member.livedoor.com/login/index",
        Map("livedoor_id" -> "testid", "password" -> "testpw"));
      List(
        assertEquals(true, page1.isInstanceOf[HtmlPage]),
        assertEquals("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n", page1.asInstanceOf[HtmlPage].source.substring(0, 103)),
        assertEquals("<html lang=\"ja\">\n", page1.asInstanceOf[HtmlPage].source.substring(103, 120)),
        assertEquals("Yahoo! JAPAN", page1.asInstanceOf[HtmlPage].select("title")(0).text),
        assertEquals(1, page1.response.cookies.size),
        assertEquals("B=", page1.response.cookies(0).substring(0, 2)),
        assertEquals(true, page2.isInstanceOf[HtmlPage]),
        assertEquals("Twitter", page2.asInstanceOf[HtmlPage].select("title")(0).text),
        assertEquals("POST", page3.asInstanceOf[HtmlPage].select("td:contains(FORMの情報) + td")(0).text),
        assertEquals("application/x-www-form-urlencoded", page3.asInstanceOf[HtmlPage].select("td:contains(FORMのタイプ) + td")(0).text),
        assertEquals("5", page3.asInstanceOf[HtmlPage].select("td:contains(FORMのバイト数) + td")(0).text),
        assertEquals("testid", page4.asInstanceOf[HtmlPage].select("#livedoor_id")(0).attr("value"))
      );
    }) :: Nil;
  }

}
