package hydrocul.hv.http;

class BinaryPage private[http] (_response: Response, _url: String)
    extends Page(_response, _url) {

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
