package hydrocul.hv.http;

import java.{ io => jio }

import hydrocul.hv.XmlElement;
import hydrocul.util.HtmlInputStreamReader;
import hydrocul.util.StreamUtil;

class HtmlPage private[http] (_response: Response, _url: String)
    extends Page(_response, _url) with HtmlElement {

  private lazy val _source: String =
    StreamUtil.stream2bin(inputStreamReader);

  override def toString(): String = {
    response.toStringDigest + "\n" +
    _source;
  }

  private def inputStreamReader: jio.Reader = {
    response.charset match {
      case Some(charset) =>
        new jio.InputStreamReader(
          new jio.ByteArrayInputStream(response.body), charset);
      case None =>
        new HtmlInputStreamReader(
          new jio.ByteArrayInputStream(response.body));
    }
  }

  private def element: HtmlElement = HtmlElementImpl.create(XmlElement.parseHtml(_source));

  def select(query: String): IndexedSeq[HtmlElement] = element.select(query);

  def outerHtml: String = element.outerHtml;

  def html: String = element.html;

  def text: String = element.text;

  def attr(name: String): String = element.attr(name);

}



