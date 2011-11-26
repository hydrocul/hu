package hydrocul.util;

import java.{ io => jio }

import hydrocul.util.{ FileUtil => obs }

object StreamUtil {

  def stream2bin(src: jio.InputStream): Array[Byte] = {
    val bop = new jio.ByteArrayOutputStream();
    obs.copyStream(src, bop);
    bop.toByteArray;
  }

  def stream2bin(src: jio.Reader): String = {
    val bop = new jio.StringWriter();
    obs.copyStream(src, bop);
    bop.toString;
  }

  def bin2stream(bin: Array[Byte]): jio.InputStream = {
    new jio.ByteArrayInputStream(bin);
  }

  def bin2stream(str: String): jio.Reader = {
    new jio.StringReader(str);
  }

  def bin2obj(bin: Array[Byte]): AnyRef = {
    ScalaUtil.using(new jio.ObjectInputStream(bin2stream(bin))){ p =>
      p.readObject();
    }
  }

  def obj2bin(obj: AnyRef): Array[Byte] = {
    val bop = new jio.ByteArrayOutputStream();
    ScalaUtil.using(new jio.ObjectOutputStream(bop)){ p =>
      p.writeObject(obj);
    }
    bop.toByteArray;
  }

}
