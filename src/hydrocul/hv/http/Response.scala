package hydrocul.hv.http;

import java.{ io => jio }

import hydrocul.hv.EncodingMania;
import hydrocul.hv.JStream;

private[http] trait Response {

  def responseHeader: Seq[(String, String)];

  def body: Array[Byte];

  def responseHeaderField(key: String): Option[String] =
    responseHeader.reverse.find(_._1 == key).map(_._2);

  def location: Option[String] =
    responseHeaderField("Location");

}

private[http] object Response {

  def apply(stream: jio.InputStream): Response = {
    throw new Exception("// TODO");
  }

  /**
   * 1行分を読み込むためのメソッドを備えたクラス。
   * 実際にバイナリを読み込むための実装は JStreamResponseReader にある。
   */
  private trait ResponseReader {

    protected def read(buf: Array[Byte], off: Int, len: Int): (Int, ResponseReader);

    protected def push(buf: Array[Byte], off: Int, len: Int): ResponseReader = {
      new BufferedResponseReader(buf, off, len, this);
    }

    /**
     * 1行を読み込む。行終端文字を含む。
     * すでにストリームの最後に達していた場合は1つ目の返り値はNone。
     */
    def readLine: (Option[String], ResponseReader) = {
      val buf = new Array[Byte](4); // TODO 大きな数字にしたほうが効率が良さそう
      val bo = new jio.ByteArrayOutputStream;
      val (f, next) = readLineSub(bo, buf);
      bo.close();
      val bin = bo.toByteArray;
      if(bin.length == 0){
        (None, next);
      } else {
        val str = EncodingMania.decodeChar(bo.toByteArray, "ISO-8859-1");
        (Some(str), next);
      }
    }

    /**
     * LFが来るまで読み込み、OutputStreamに書きだす。
     * 読み込んだ結果ストリームの最後に達した場合は1つ目の返り値はtrue。
     */
    private def readLineSub(bo: jio.OutputStream, buf: Array[Byte]): (Boolean, ResponseReader) = {
      val (l, f, next) = readLineSub2(buf, 0, buf.length);
      if(l < 0){
        (true, next);
      } else {
        bo.write(buf, 0, l);
        if(f){
          (false, next);
        } else {
          next.readLineSub(bo, buf);
        }
      }
    }

    /**
     * 読み込むが、LFが来たらそこでやめる。LFが来たら読み込んだ結果の最後はLFか、
     * ただし、ストリームの最後に達した場合は最後はLFとは限らない。
     * LFまたはストリームの最後に達した場合は2つ目の返り値はtrue、
     * まだ続きがある場合はfalse。
     */
    private def readLineSub2(buf: Array[Byte], off: Int, len: Int): (Int, Boolean, ResponseReader) = {
      val (l, next) = read(buf, off, len);
      if(l < 0){
        (l, true, next); // ストリームの最後だった場合
      } else {
        (0 until l).find { i => buf(off + i) == '\n'; } match {
          case Some(i) => // LFが見つかった場合
            val next2 = next.push(buf, off + i + 1, len - i -1);
            ((i + 1), true, next2);
          case None => // LFが見つからなかった場合
            (l, false, next);
        }
      }
    }

  }

  private class JStreamResponseReader(stream: JStream[Byte]) extends ResponseReader {

    protected def read(buf: Array[Byte], off: Int, len: Int): (Int, ResponseReader) = {
      val (l, next) = stream.read(buf, off, len);
      (l, new JStreamResponseReader(next));
    }

  }

  private class BufferedResponseReader(headBuf: Array[Byte], headOff: Int, headLen: Int,
      tail: ResponseReader) extends ResponseReader {

    protected def read(buf: Array[Byte], off: Int, len: Int): (Int, ResponseReader) = {
      if(len < headLen){
        System.arraycopy(headBuf, headOff, buf, off, len);
        (len, new BufferedResponseReader(headBuf, headOff + len, headLen - len, tail));
      } else {
        System.arraycopy(headBuf, headOff, buf, off, headLen);
        (headLen, tail);
      }
    }

  }

  private[http] def test(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;

    {
      val str = "abc\r\n123\r\n\r\nABC\r\nDEF";
      val inputStream = new jio.ByteArrayInputStream(EncodingMania.encodeChar(str, "ISO-8859-1"));
      val jstream = JStream.fromJava(inputStream);
      val reader1 = new JStreamResponseReader(jstream);
      val (actual1, reader2) = reader1.readLine;
      val (actual2, reader3) = reader2.readLine;
      val (actual3, reader4) = reader3.readLine;
      val (actual4, reader5) = reader4.readLine;
      val (actual5, reader6) = reader5.readLine;
      val (actual6, _      ) = reader6.readLine;
      List(
        assertEquals(Some("abc\r\n"), actual1),
        assertEquals(Some("123\r\n"), actual2),
        assertEquals(Some("\r\n"),    actual3),
        assertEquals(Some("ABC\r\n"), actual4),
        assertEquals("4142430d0a",
          EncodingMania.encodeHex(EncodingMania.encodeChar(actual4.get, "UTF-8"))),
        assertEquals(Some("DEF"),     actual5),
        assertEquals(None,            actual6)
      );
    }

  }

}
