package hydrocul.hv.http;

private[http] trait Response {

  def responseHeader: Seq[(String, String)];

  def body: Array[Byte];

  def responseHeaderField(key: String): Option[String] =
    responseHeader.reverse.find(_._1 == key).map(_._2);

  def location: Option[String] =
    responseHeaderField("Location");

}
