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

  def binaryToSha1(data: Array[Byte]): Array[Byte] = {
    val shax = MessageDigest.getInstance("SHA-1");
    shax.reset();
    shax.update(data);
    val key = shax.digest();
    if(key.length != 20){
      throw new AssertionError(key.map("%02x".format(_)).mkString);
    }
    key;
  }

  def binaryToSha256(data: Array[Byte]): Array[Byte] = {
    val shax = MessageDigest.getInstance("SHA-256");
    shax.reset();
    shax.update(data);
    val key = shax.digest();
    if(key.length != 32){
      throw new AssertionError(key.map("%02x".format(_)).mkString);
    }
    key;
  }

  def binaryToHash(data: Array[Byte]): Array[Byte] = binaryToSha1(data);

  private[hv] def test(): Seq[Option[String]] = {
    import TestLib._;
    {
      val a = encodeCipher("abc".getBytes, "あいうえお".getBytes("UTF-8"));
      val b = new String(decodeCipher("abc".getBytes, a), "UTF-8");
      List(
        assertEquals("07b91211306447be8244146ad5d07a03", encodeHex(a)),
        assertEquals("あいうえお", b)
      );
    } ++
    {
      val actual = encodeHex(binaryToSha256("abc\n".getBytes));
      val expected = "edeaaff3f1774ad2888673770c6d64097e391bc362d7d6fb34982ddf0efd18cb";
      List(
        assertEquals(expected, actual)
      );
    } ++
    {
      val actual = encodeHex(binaryToSha256("1234".getBytes));
      val expected = "03ac674216f3e15c761ee1a5e255f067953623c8b388b4459e13f978d7c846f4";
      List(
        assertEquals(expected, actual)
      );
    }
  }

}
