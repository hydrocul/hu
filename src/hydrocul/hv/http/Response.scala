package hydrocul.hv.http;

import java.{ io => jio }

import hydrocul.hv.EncodingMania;
import hydrocul.hv.JStream;
import hydrocul.util.StreamUtil;

private[http] trait Response {

  def statusCode: Int;

  def responseHeader: Seq[(String, String)];

  def body: Array[Byte];

  def responseHeaderField(key: String): Option[String] =
    responseHeader.reverse.find(_._1 == key).map(_._2);

  def location: Option[String] =
    responseHeaderField("Location");

  def contentTypeField: Option[String] =
    responseHeaderField("Content-Type");

  private lazy val _contentTypeField = {
    contentTypeField match {
      case Response.ContentTypePattern(t, cs) =>
        Some((t.trim, cs.trim));
      case _ =>
        None;
    }
  }

  def contentType: Option[String] =
    _contentTypeField.map(_._1);

  def charset: Option[String] =
    _contentTypeField.map(_._2);

  def toStringDigest: String = {
    "StatusCode: " + statusCode + "\n"
    location.map("Location: " + _ + "\n").getOrElse("") +
    contentType.map("ContentType: " + _ + "\n").getOrElse("") +
    charset.map("Charset: " + _ + "\n").getOrElse("");
  }

}

private[http] object Response {

  def apply(stream: jio.InputStream): Response = {
    val reader = new JStreamResponseReader(JStream.fromJava(stream));
    val (code, reader2) = parseStatusLine(reader);
    val (header, reader3) = parseHeaderLines(Nil, reader2);
    val b = StreamUtil.stream2bin(reader3.toJavaInputStream);
    new Response {
      def statusCode = code;
      def responseHeader = header;
      def body = b;
    }
  }

  private def parseStatusLine(reader: ResponseReader): (Int, ResponseReader) = {
    reader.readLine match {
      case (Some(line), nextReader) =>
        val a = line.split(" +", 3);
        if(a.size < 3){
          throw new Exception("line: %s".format(line));
        }
        val code = try {
          a(1).toInt;
        } catch { case _ =>
          throw new Exception("line: %s".format(line));
        }
        (code, nextReader);
      case _ =>
        throw new Exception();
    }
  }

  /**
   * レスポンスヘッダの全行と本文を取得する。
   */
  private def parseHeaderLines(responseHeader: List[(String, String)], reader: ResponseReader):
      (Seq[(String, String)], ResponseReader) = {
    reader.readLine match {
      case (Some(line), nextReader) =>
        if(line=="\r\n"){
          (responseHeader.reverse, nextReader);
        } else {
          val p = line.indexOf(':');
          val (h, v) = if(p < 0){
            (line.trim, "");
          } else {
            (line.substring(0, p).trim, line.substring(p + 1).trim);
          }
          parseHeaderLines((h, v) :: responseHeader, nextReader);
        }
      case (None, nextReader) =>
        (responseHeader.reverse, nextReader);
    }
  }

  /**
   * 1行分を読み込むためのメソッドを備えたクラス。
   * 実際にバイナリを読み込むための実装は JStreamResponseReader にある。
   */
  private trait ResponseReader {

    def read(buf: Array[Byte], off: Int, len: Int): (Int, ResponseReader);

    def closeIO();

    def push(buf: Array[Byte], off: Int, len: Int): ResponseReader = {
      new BufferedResponseReader(buf, off, len, this);
    }

    def toJavaInputStream: jio.InputStream = new jio.InputStream {

      private var reader = ResponseReader.this;

      override def read(): Int = {
        val buf = new Array[Byte](1);
        val l = read(buf, 0, 1);
        if(l < 0){
          -1;
        } else if(l == 0){
          read();
        } else {
          buf(0);
        }
      }

      override def read(buf: Array[Byte], off: Int, len: Int): Int = {
        val (l, next) = reader.read(buf, off, len);
        reader = next;
        l;
      }

      override def close(){
        reader.closeIO();
      }

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
            val next2 = next.push(buf, off + i + 1, l - i -1);
            ((i + 1), true, next2);
          case None => // LFが見つからなかった場合
            (l, false, next);
        }
      }
    }

  }

  private class JStreamResponseReader(stream: JStream[Byte]) extends ResponseReader {

    def read(buf: Array[Byte], off: Int, len: Int): (Int, ResponseReader) = {
      val (l, next) = stream.read(buf, off, len);
      (l, new JStreamResponseReader(next));
    }

    def closeIO(){
      stream.closeIO();
    }

  }

  private class BufferedResponseReader(headBuf: Array[Byte], headOff: Int, headLen: Int,
      tail: ResponseReader) extends ResponseReader {

    def read(buf: Array[Byte], off: Int, len: Int): (Int, ResponseReader) = {
      if(len < headLen){
        System.arraycopy(headBuf, headOff, buf, off, len);
        (len, new BufferedResponseReader(headBuf, headOff + len, headLen - len, tail));
      } else {
        System.arraycopy(headBuf, headOff, buf, off, headLen);
        (headLen, tail);
      }
    }

    def closeIO() = tail.closeIO();

  }

  private val ContentTypePattern = "([^;]+);(.+)".r;

  private[http] def test(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;

    {
      val str = "abc\r\n" + "123\r\n" + "\r\n" + "ABC\r\n" + "DEF";
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
