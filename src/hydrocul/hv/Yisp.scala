package hydrocul.hv;

class Yisp private(dir: String){

  def put(bin: Array[Byte]): String = {
    val hash = CipherUtil.binaryToSha256(bin);
    val key = EncodingMania.encodeHex(hash);
    val path = getPath(key);
    File(path).write(Some(bin));
    key;
  }

  def get(key: String): Array[Byte] = {
    val path = getPath(key);
    File(path).read.get;
  }

  private def getPath(key: String): String =
    dir + "/" + key.substring(0, 2) + "/" + key.substring(2, 4) + "/" + key.substring(4);

}

object Yisp {

  def apply(): Yisp = {
    val home = System.getProperty("user.home");
    apply(home + "/.hu/yispfiles");
  }

  def apply(dir: String) = new Yisp(dir);

}

