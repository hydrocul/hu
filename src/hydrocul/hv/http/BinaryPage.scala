package hydrocul.hv.http;

class BinaryPage private[http] (response: Response, url: String)
    extends Page(response, url) {

  def bin: Array[Byte] = response.body;

  override def toString(): String = {
    response.toStringDigest + "\n" +
    (if(bin.length > 60){
      bin.take(60).map("%02x".format(_)).mkString("", " ", "...");
    } else {
      bin.map("%02x".format(_)).mkString("", " ", "");
    });
  }

}
