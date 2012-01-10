package hydrocul.hv.http;

class WebBrowser {

  def doGetIO(url: String): Page = {
    val urlInfo = UrlInfo(url);
    val response = Sockets.doGetIO(urlInfo.host, urlInfo, Map.empty, Request.defaultHeader);
    new BinaryPage(response, url);
  }

  // def doPostIO(url: UrlInfo, postParam: Map[String, String]): Page;

}

object WebBrowser {

  private[hv] def test(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;
    UrlInfo.test() ++
    Response.test();
  }

}
