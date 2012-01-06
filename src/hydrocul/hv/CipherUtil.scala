package hydrocul.hv;

import java.security.MessageDigest;
import javax.crypto;

object CipherUtil {

  def encodeCipher(passwd: Array[Byte], input: Array[Byte]): Array[Byte] = {
    val spec = new crypto.spec.SecretKeySpec(passwd, "Blowfish");
    val cipher = crypto.Cipher.getInstance("Blowfish");
    cipher.init(crypto.Cipher.ENCRYPT_MODE, spec);
    cipher.doFinal(input);
  }

  def decodeCipher(passwd: Array[Byte], input: Array[Byte]): Array[Byte] = {
    val spec = new crypto.spec.SecretKeySpec(passwd, "Blowfish");
    val cipher = crypto.Cipher.getInstance("Blowfish");
    cipher.init(crypto.Cipher.DECRYPT_MODE, spec);
    cipher.doFinal(input);
  }

  def encodeHex(input: Array[Byte]): String = {
    EncodingMania.encodeHex(input);
  }

  def decodeHex(input: String): Array[Byte] = {
    EncodingMania.decodeHex(input);
  }

  def binaryToHash(data: Array[Byte]): Array[Byte] = {
    val sha1 = MessageDigest.getInstance("SHA-1");
    sha1.reset();
    sha1.update(data);
    val key = sha1.digest();
    if(key.length != 20){
      throw new AssertionError(key.map("%02x".format(_)).mkString);
    }
    key;
  }

  private[hv] def test(): Seq[Option[String]] = {
    import TestLib._;
    val a = encodeCipher("abc".getBytes, "あいうえお".getBytes("UTF-8"));
    val b = new String(decodeCipher("abc".getBytes, a), "UTF-8");
    List(
      assertEquals("07b91211306447be8244146ad5d07a03", encodeHex(a)),
      assertEquals("あいうえお", b)
    );
  }

}
