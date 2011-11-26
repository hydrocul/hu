package hydrocul.hu;

import java.{ net => jnet }

import hydrocul.util.StreamUtil;

object UrlConnection {

  def getRaw(url: String) = IO[Option[Array[Byte]]](){
    try {
      val url2 = new jnet.URL(url);
      val conn = url2.openConnection();
      conn.connect();
      val bin = StreamUtil.stream2bin(conn.getInputStream);
      Some(bin);
    } catch { case e =>
      None;
    }
  }

  def getString(url: String): IO[Option[String]] = getString(url, None);

  def getString(url: String, charEncoding: Option[String]) = IO[Option[String]](){
    try {
      val url2 = new jnet.URL(url);
      val conn = url2.openConnection();
      conn.connect();
      val enc = Option(conn.getContentEncoding);
      val bin = StreamUtil.stream2bin(conn.getInputStream);
      val str = (charEncoding, enc) match {
        case (Some(e), _) => EncodingMania.decodeChar(bin, e);
        case (_, Some(e)) => EncodingMania.decodeChar(bin, e);
        case _ => EncodingMania.decodeChar(bin, "UTF-8");
      }
      Some(str);
    } catch { case e =>
      None;
    }
  }

}
