package hydrocul.hv.http;

class WebBrowser private () {

  def doGetIO(url: String): Page = {
    val urlInfo = UrlInfo(url);
    val response = Sockets.doGetIO(urlInfo.host, urlInfo, Map.empty, Request.defaultHeader);
    response.contentType match {
      case Some("text/html") =>
        new HtmlPage(response, url);
      case _ =>
        new BinaryPage(response, url);
    }
  }

  // def doPostIO(url: UrlInfo, postParam: Map[String, String]): Page;

}

object WebBrowser {

  def create(): WebBrowser = new WebBrowser();

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
      val page = browser.doGetIO("http://www.yahoo.co.jp/");
      List(
        assertEquals(true, page.isInstanceOf[HtmlPage]),
        assertEquals("Yahoo! JAPAN", page.asInstanceOf[HtmlPage].element.select("title")(0).text)
      );
    }) :: Nil;
  }

}
