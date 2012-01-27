package hydrocul.hv.http;

import java.{ io => jio }

import hydrocul.hv.Json;
import hydrocul.hv.XmlElement;
import hydrocul.util.HtmlInputStreamReader;
import hydrocul.util.StreamUtil;

class JsonPage private[http] (_response: Response, _url: String)
    extends Page(_response, _url) {

  private lazy val _source: String =
    StreamUtil.stream2bin(inputStreamReader);

  private lazy val json: Any = Json.decode(_source);

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

  def value: Any = json;

}



