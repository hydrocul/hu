package hydrocul.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;

object FileUtil {

  def deleteDirectory(dir: File){
    val list = dir.list;
    if(list!=null){
      list.foreach(c => deleteDirectory(new File(dir, c)));
    }
    dir.delete();
  }

  /**
   * InputStream から OutputStream にコピーする。
   * 最後にそれぞれを close する。
   */
  def copyStream(src: InputStream, dst: OutputStream){
    val srcBuf = new BufferedInputStream(src);
    try {
      val dstBuf = new BufferedOutputStream(dst);
      try {
        val buf = new Array[Byte](1024);
        var len = srcBuf.read(buf, 0, buf.length);
        while(len >= 0){
          dstBuf.write(buf, 0, len);
          len = srcBuf.read(buf, 0, buf.length);
        }
      } finally {
        dstBuf.close();
      }
    } finally {
      srcBuf.close();
    }
  }

  /**
   * InputStream から OutputStream にコピーする。
   */
  def copyStreamWithoutClosing(src: InputStream, dst: OutputStream){
    val srcBuf = new BufferedInputStream(src);
    val dstBuf = new BufferedOutputStream(dst);
    val buf = new Array[Byte](1024);
    var len = srcBuf.read(buf, 0, buf.length);
    while(len >= 0){
      dstBuf.write(buf, 0, len);
      len = srcBuf.read(buf, 0, buf.length);
    }
  }

  /**
   * Reader から Writer にコピーする。
   * 最後にそれぞれを close する。
   */
  def copyStream(src: Reader, dst: Writer){
    val srcBuf = new BufferedReader(src);
    try {
      val dstBuf = new BufferedWriter(dst);
      try {
        val buf = new Array[Char](1024);
        var len = srcBuf.read(buf, 0, buf.length);
        while(len >= 0){
          dstBuf.write(buf, 0, len);
          len = srcBuf.read(buf, 0, buf.length);
        }
      } finally {
        dstBuf.close();
      }
    } finally {
      srcBuf.close();
    }
  }

  def stream2bin(src: InputStream): Array[Byte] = {
    val bop = new ByteArrayOutputStream();
    copyStream(src, bop);
    bop.toByteArray;
  }

  def bin2stream(bin: Array[Byte], dst: OutputStream){
    ScalaUtil.using(dst){ p =>
      p.write(bin);
    }
  }

  def readObjectFromFile(file: File): AnyRef = {
    ScalaUtil.using(new ObjectInputStream(new FileInputStream(file))){ p =>
      p.readObject();
    }
  }

  def writeObjectToFile(obj: Any, file: File){
    ScalaUtil.using(new ObjectOutputStream(new FileOutputStream(file))){ p =>
      p.writeObject(obj);
    }
  }

}

