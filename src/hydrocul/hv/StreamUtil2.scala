package hydrocul.hv;

import java.{ io => jio }

object StreamUtil2 {

  /**
   * Javaの InputStream をScalaの Stream に変換する。
   * Stream の最後まで読み取ったら自動で InputStream#close が呼び出される。
   */
  def streamJava2Scala(src: jio.InputStream): (()=>Stream[Array[Byte]], jio.Closeable) =
    streamJava2ScalaSub(src, 1024);

  private def streamJava2ScalaSub(src: jio.InputStream, buflen: Int):
      (()=>Stream[Array[Byte]], jio.Closeable) =
    (ByteJavaStream(src, new Array[Byte](buflen)).toScalaStream, src);

  /**
   * Javaの Reader をScalaの Stream に変換する。
   * Stream の最後まで読み取ったら自動で Reader#close が呼び出される。
   */
  def streamJava2Scala(src: jio.Reader): (()=>Stream[Array[Char]], jio.Closeable) =
    streamJava2ScalaSub(src, 1024);

  private def streamJava2ScalaSub(src: jio.Reader, buflen: Int):
      (()=>Stream[Array[Char]], jio.Closeable) =
    (CharJavaStream(src, new Array[Char](buflen)).toScalaStream, src);

  private trait JavaStream[A] {

    protected def read(): Option[(Array[A], JavaStream[A])];

    private lazy val stream: Option[(Array[A], JavaStream[A])] = read();

    def toScalaStream: ()=>Stream[Array[A]] = { () =>
      stream match {
        case Some((bin, nextStream)) =>
          Stream.cons(bin, nextStream.toScalaStream());
        case None =>
          Stream.empty;
      }
    }

  }

  private def ByteJavaStream(src: jio.InputStream, buf: Array[Byte]): JavaStream[Byte] =
      new JavaStream[Byte]{
    override protected def read(): Option[(Array[Byte], JavaStream[Byte])] = {
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
    override protected def read(): Option[(Array[Char], JavaStream[Char])] = {
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

  def streamScala2JavaB(src: ()=>Stream[Array[Byte]], closable: jio.Closeable):
    jio.InputStream = new jio.InputStream {

    private var stream = src;

    override def read(): Int = {
      val s = stream();
      if(s.isEmpty)
        return -1;
      val h = s.head;
      if(h.length == 0){
        stream = { () => s.tail; }
        read();
      } else if(h.length == 1){
        val ret = h(0);
        stream = { () => s.tail; }
        ret;
      } else {
        val ret = h(0);
        val buf2 = new Array[Byte](h.length - 1);
        System.arraycopy(h, 1, buf2, 0, buf2.length);
        stream = { () => Stream.cons(buf2, s.tail); }
        ret;
      }
    }

    override def read(buf: Array[Byte], off: Int, len: Int): Int = {
      val s = stream();
      if(s.isEmpty)
        return -1;
      val h = s.head;
      if(h.length == 0){
        stream = { () => s.tail; }
        read(buf, off, len);
      } else if(h.length <= len){
        System.arraycopy(h, 0, buf, off, h.length);
        stream = { () => s.tail; }
        h.length;
      } else {
        System.arraycopy(h, 0, buf, off, len);
        val buf2 = new Array[Byte](h.length - len);
        System.arraycopy(h, len, buf2, 0, buf2.length); // TODO 配列のコピーは高コスト
        stream = { () => Stream.cons(buf2, s.tail); }
        len;
      }
    }

    override def close() = closable.close();

  }

  def streamScala2JavaC(src: ()=>Stream[Array[Char]], closable: jio.Closeable):
    jio.Reader = new jio.Reader {

    private var stream = src;

    override def read(buf: Array[Char], off: Int, len: Int): Int = {
      val s = stream();
      if(s.isEmpty)
        return -1;
      val h = s.head;
      if(h.length == 0){
        stream = { () => s.tail; }
        read(buf, off, len);
      } else if(h.length <= len){
        System.arraycopy(h, 0, buf, off, h.length);
        stream = { () => s.tail; }
        h.length;
      } else {
        System.arraycopy(h, 0, buf, off, len);
        val buf2 = new Array[Char](h.length - len);
        System.arraycopy(h, len, buf2, 0, buf2.length); // TODO 配列のコピーは高コスト
        stream = { () => Stream.cons(buf2, s.tail); }
        len;
      }
    }

    override def close() = closable.close();

  }

  private[hv] def test(): Seq[Option[String]] = {
    import TestLib._;
    import hydrocul.util.StreamUtil._;

    {
      // jio.Reader からScalaの Stream への変換のテスト

      val javaStream = new jio.StringReader("あいうえお");
      val expected = List(List('あ', 'い'), List('う', 'え'), List('お'));
      val scalaStream = streamJava2ScalaSub(javaStream, 2)._1;

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
    } ++ {
      // Scalaの Stream から jio.Reader への変換のテスト

      val scalaStream = List(List('あ', 'い'), List('う')).map(_.toArray).toStream;
      val javaStream = streamScala2JavaC(()=>scalaStream, new jio.Closeable {
        def close(){}
      });
      val actual = stream2bin(javaStream);
      val expected = "あいう";
      List(
        assertEquals(expected, actual)
      );
    }

  }

}
