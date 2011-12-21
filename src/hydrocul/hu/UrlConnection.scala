package hydrocul.hu;

import java.{ net => jnet }

import hydrocul.util.StreamUtil;

object UrlConnection {

  def getRaw(url: String): IO[Option[Array[Byte]]] = {
    IO(){
      try {
        val url2 = new jnet.URL(url);
        val conn = url2.openConnection();
        conn.connect();
        Some(conn);
      } catch { case e =>
        None;
      }
    } then {
      case Some(conn) =>
        Option(conn.getHeaderField("Location")) match {
          case Some(location) => getRaw(location);
          case None =>
            val bin = StreamUtil.stream2bin(conn.getInputStream);
            IO()(Some(bin));
        }
      case None =>
        IO()(None);
    }
  }

  def getString(url: String): IO[Option[String]] = getString(url, None);

  def getString(url: String, charEncoding: Option[String]): IO[Option[String]] = {
    IO(){
      try {
        val url2 = new jnet.URL(url);
        val conn = url2.openConnection();
        conn.connect();
        Some(conn);
      } catch { case e =>
        None;
      }
    } then {
      case Some(conn) =>
        Option(conn.getHeaderField("Location")) match {
          case Some(location) => getString(location, charEncoding);
          case None =>
            val enc = Option(conn.getContentEncoding);
            val bin = StreamUtil.stream2bin(conn.getInputStream);
            val str = (charEncoding, enc) match {
              case (Some(e), _) => EncodingMania.decodeChar(bin, e);
              case (_, Some(e)) => EncodingMania.decodeChar(bin, e);
              case _ => EncodingMania.decodeChar(bin, "UTF-8");
            }
            IO()(Some(str));
        }
      case None =>
        IO()(None);
    }
  }

}
