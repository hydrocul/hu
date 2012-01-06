package hydrocul.hv;

import java.{ io => jio };
import java.util.concurrent.atomic.AtomicBoolean;
import javax.crypto;

import org.apache.commons.codec;

import hydrocul.util.WriterOutputStream;

object EncodingMania {

  def thru(input: String): Array[Byte] = {
    val buf = new Array[Byte](input.length);
    (0 until input.length).foreach { ii =>
      buf(ii) = input.charAt(ii).asInstanceOf[Byte];
    }
    buf;
  }

  def thru(input: Array[Byte]): Array[Char] = {
    input.map(_.asInstanceOf[Char]);
  }

  def encodeChar(input: String, charEncoding: String): Array[Byte] = {
    val op = new jio.ByteArrayOutputStream();
    val op2 = new jio.OutputStreamWriter(op, charEncoding);
    op2.write(input);
    op2.close();
    op.toByteArray;
  }

  def decodeChar(input: Array[Byte], charEncoding: String): String = {
    val op = new jio.StringWriter();
    val op2 = new WriterOutputStream(op, charEncoding);
    op2.write(input);
    op2.close();
    op.toString;
  }

  def encodeHex(input: Array[Byte]): String = {
    input.map("%02x".format(_)).mkString;
  }

  def decodeHex(input: String): Array[Byte] = {
    val size = input.length;
    if(size % 2 != 0){
      throw new IllegalArgumentException(input);
    }
    val arr = new Array[Byte](size / 2);
    (0 until arr.length).foreach { i =>
      val i2 = i * 2;
      arr(i) = Integer.parseInt(input.substring(i2, i2 + 2), 16).
          asInstanceOf[Byte];
    }
    arr;
  }

  def encodeUrl(input: String, charEncoding: String): String = {
    java.net.URLEncoder.encode(input, charEncoding);
  }

  def decodeUrl(input: String, charEncoding: String): String = {
    java.net.URLDecoder.decode(input, charEncoding);
  }

  def encodeBase64(input: Array[Byte], divideIntoLines: Boolean = true): String = {
    val dst = new jio.ByteArrayOutputStream;
    val output = if(divideIntoLines)
      new codec.binary.Base64OutputStream(dst, true);
    else
      new codec.binary.Base64OutputStream(dst, true, 0, null);
    output.write(input);
    output.close();
    new String(dst.toByteArray.map(_.asInstanceOf[Char]));
  }

  def decodeBase64(input: String): Array[Byte] = {
    val dst = new jio.ByteArrayOutputStream;
    val output = new codec.binary.Base64OutputStream(dst, false);
    output.write((0 until input.length).map(i => input.charAt(i).asInstanceOf[Byte]).toArray);
    output.close();
    dst.toByteArray;
  }

  def encodeCipher(input: Array[Byte], passwd: Array[Byte]): Array[Byte] =
    CipherUtil.encodeCipher(passwd, input);

  def decodeCipher(input: Array[Byte], passwd: Array[Byte]): Array[Byte] =
    CipherUtil.decodeCipher(passwd, input);

}
