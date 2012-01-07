package hydrocul.hv;

import java.{ io => jio }

object StreamUtil2 {

  /**
   * Javaの InputStream をScalaの Stream に変換する。
   * Stream の最後まで読み取ったら自動で InputStream#close が呼び出される。
   */
  def streamJava2Scala(src: jio.InputStream): ()=>Stream[Array[Byte]] =
    streamJava2ScalaSub(src, 1024);

  private def streamJava2ScalaSub(src: jio.InputStream, buflen: Int): ()=>Stream[Array[Byte]] =
    ByteJavaStream(src, new Array[Byte](buflen)).toScalaStreamIO;

  /**
   * Javaの Reader をScalaの Stream に変換する。
   * Stream の最後まで読み取ったら自動で Reader#close が呼び出される。
   */
  def streamJava2Scala(src: jio.Reader): ()=>Stream[Array[Char]] =
    streamJava2ScalaSub(src, 1024);

  private def streamJava2ScalaSub(src: jio.Reader, buflen: Int): ()=>Stream[Array[Char]] =
    CharJavaStream(src, new Array[Char](buflen)).toScalaStreamIO;

  private trait JavaStream[A] {

    protected def readIO(): Option[(Array[A], JavaStream[A])];

    private lazy val stream: Option[(Array[A], JavaStream[A])] = readIO();

    def toScalaStreamIO: ()=>Stream[Array[A]] = { () =>
      stream match {
        case Some((bin, nextStream)) =>
          Stream.cons(bin, nextStream.toScalaStreamIO());
        case None =>
          Stream.empty;
      }
    }

  }

  private def ByteJavaStream(src: jio.InputStream, buf: Array[Byte]): JavaStream[Byte] =
      new JavaStream[Byte]{
    override protected def readIO(): Option[(Array[Byte], JavaStream[Byte])] = {
      val len = src.read(buf);
      if(len < 0){
        src.close();
        None;
      } else {
        val buf2 = new Array[Byte](len);
        System.arraycopy(buf, 0, buf2, 0, len);
        Some((buf2, ByteJavaStream(src, buf)));
      }
    }
  }

  private def CharJavaStream(src: jio.Reader, buf: Array[Char]): JavaStream[Char] =
      new JavaStream[Char]{
    override protected def readIO(): Option[(Array[Char], JavaStream[Char])] = {
      val len = src.read(buf);
      if(len < 0){
        src.close();
        None;
      } else {
        val buf2 = new Array[Char](len);
        System.arraycopy(buf, 0, buf2, 0, len);
        Some((buf2, CharJavaStream(src, buf)));
      }
    }
  }

  private[hv] def test(): Seq[Option[String]] = {
    import TestLib._;

    val javaStream = new jio.StringReader("あいうえお");
    val expected = List(List('あ', 'い'), List('う', 'え'), List('お'));
    val scalaStream = streamJava2ScalaSub(javaStream, 2);

    val actual1 = scalaStream().map(_.toList);

    // scalaStreamを複数回呼び出してもよいことのテスト
    val actual2 = scalaStream().map(_.toList);

    val s = scalaStream().force;
    val expected3 = List(List('う', 'え'), List('お'));
    val actual3 = s.tail.map(_.toList);

    List(
      assertEquals(expected, actual1),
      assertEquals(expected, actual2),
      assertEquals(expected3, actual3)
    );

  }

}
