package hydrocul.hv.http;

abstract // TODO
class WebBrowser {

  def doGetIO(url: UrlInfo): Page;

  def doPostIO(url: UrlInfo, postParam: Map[String, String]): Page;

}

object WebBrowser {

  private[hv] def test(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;
    UrlInfo.test() ++
    Response.test();
  }

}
