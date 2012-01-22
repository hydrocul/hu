package hydrocul.hv;

//import hydrocul.hv.CipherUtil;
//import hydrocul.hv.EncodingMania;
import hydrocul.util.StreamUtil;

trait KeyValueDir {

  def doPut(key: AnyRef, value: Option[AnyRef]);

  def doGet(key: AnyRef): Option[AnyRef];

}

object KeyValueDir {

  def create(dir: File): KeyValueDir = new Impl(dir);

  private class Impl(val dir: File) extends KeyValueDir {

    def doPut(key: AnyRef, value: Option[AnyRef]){
      val keyBin = StreamUtil.obj2bin(key);
      val hash = EncodingMania.encodeHex(CipherUtil.binaryToHash(keyBin));
      val path = hash.substring(0, 2) + "/" + hash.substring(2, 4) + "/" + hash.substring(4) + ".dat";
      val file = dir.getByPath(path).get;
      val valBin = value.map(value => StreamUtil.obj2bin(value));
      file.write(valBin);
    }

    def doGet(key: AnyRef): Option[AnyRef] = {
      val keyBin = StreamUtil.obj2bin(key);
      val hash = EncodingMania.encodeHex(CipherUtil.binaryToHash(keyBin));
      val path = hash.substring(0, 2) + "/" + hash.substring(2, 4) + "/" + hash.substring(4) + ".dat";
      val file = dir.getByPath(path).get;
      file.read.map(valBin => StreamUtil.bin2obj(valBin));
    }

  }

}

