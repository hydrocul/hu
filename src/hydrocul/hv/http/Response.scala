package hydrocul.hv.http;

import java.{ io => jio }

private[http] trait Response {

  def responseHeader: Seq[(String, String)];

  def body: Array[Byte];

  def responseHeaderField(key: String): Option[String] =
    responseHeader.reverse.find(_._1 == key).map(_._2);

  def location: Option[String] =
    responseHeaderField("Location");

}

private[http] object Response {

  def apply(stream: jio.InputStream): Response = {
    throw new Exception("// TODO");
  }

}
