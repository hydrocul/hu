package hydrocul.hv.http;

class WebBrowser private () {

  def doGet(url: String): Page = {
    val urlInfo = UrlInfo(url);
    val response = Sockets.doGet(urlInfo.host, urlInfo, Map.empty, Request.defaultHeader);
    response.contentType match {
      case Some("text/html") =>
        new HtmlPage(response, url);
      case Some("application/json") =>
        new JsonPage(response, url);
      case _ =>
        new BinaryPage(response, url);
    }
  }

  // def doPost(url: UrlInfo, postParam: Map[String, String]): Page;

}

object WebBrowser {

  def create(): WebBrowser = new WebBrowser();

  def doGet(url: String): Page = create().doGet(url);

  private[hv] def test(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    import hydrocul.hv.TestLib._;
    testSub(all) ++
    List[(String, Function0[Seq[Option[String]]])](
      ("http.UrlInfo", UrlInfo.test),
      ("http.Response", Response.test)
    );
  }

  private def testSub(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    import hydrocul.hv.TestLib._;
    ("http.Webbrowser", { () =>
      val browser = WebBrowser.create();
      val page1 = browser.doGet("http://www.yahoo.co.jp/");
      val page2 = browser.doGet("https://twitter.com/");
      List(
        assertEquals(true, page1.isInstanceOf[HtmlPage]),
        assertEquals("Yahoo! JAPAN", page1.asInstanceOf[HtmlPage].select("title")(0).text),
        assertEquals(true, page2.isInstanceOf[HtmlPage])
        // assertEquals("DEBUG", page2.asInstanceOf[HtmlPage].select("title")(0).text)
      );
    }) :: Nil;
  }

}
