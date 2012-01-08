package hydrocul.hv;

import java.{ io => jio }

trait JStream[A] extends jio.Closeable {

  /**
   * 次のlenサイズを読み込み、その結果と次を読み込むためのJStreamインスタンスを返す。
   * 返される配列はこのJStreamインスタンス自信がイミュータブルオブジェクトとして
   * 再利用する可能性があるので、要素を変更してはいけない。
   * 返される配列はlenを超える可能性も下回る可能性もある。
   */
  def read(len: Int): Option[(Array[A], JStream[A])];

  def read(buf: Array[A], off: Int, len: Int): (Int, JStream[A]);

  def close();

  def toJavaB(implicit ev: A <:< Byte): jio.InputStream = {
    throw new Exception("// TODO");
  }

  def toJavaC(implicit ev: A <:< Char): jio.Reader = {
    var jstream = this;
    new jio.Reader {

      override def read(buf: Array[Char], off: Int, len: Int): Int = {
        val t = jstream.read(buf.asInstanceOf[Array[A]], off, len);
        jstream = t._2;
        t._1;
      }

      override def close() = jstream.close();

    }
  }

}

object JStream {

  def fromJava(src: jio.InputStream): JStream[Byte] = {
    new JStreamImpl[Byte]({ (buf: Array[Byte], off: Int, len: Int) =>
      src.read(buf, off, len);
    }, src);
  }

  def fromJava(src: jio.Reader): JStream[Char] = {
    new JStreamImpl[Char]({ (buf: Array[Char], off: Int, len: Int) =>
      src.read(buf, off, len);
    }, src);
  }

  private class JStreamImpl[A](p: (Array[A],Int,Int)=>Int,
      closable: jio.Closeable)(implicit manifest: Manifest[A])
      extends JStream[A] {

    private var t: Option[Option[(Array[A], Int, Int, JStreamImpl[A])]] = None;

    def read(len: Int): Option[(Array[A], JStream[A])] = {
      if(t.isDefined && !t.get.isDefined){
        None;
      } else if(t.isDefined){
        val t2 = t.get.get;
        Some((t2._1, t2._4));
      } else {
        val buf = new Array[A](len);
        val l = p(buf, 0, len);
        if(l < 0){
          t = Some(None);
          None;
        } else if(l >= len){
          val t2 = (buf, 0, len, new JStreamImpl(p, closable));
          t = Some(Some(t2));
          Some((buf, t2._4));
        } else {
          val b = new Array[A](l);
          System.arraycopy(buf, 0, b, 0, l);
          val t2 = (b, 0, l, new JStreamImpl(p, closable));
          t = Some(Some(t2));
          Some((b, t2._4));
        }
      }
    }

    def read(buf: Array[A], off: Int, len: Int): (Int, JStream[A]) = {
      if(t.isDefined && !t.get.isDefined){
        (-1, this);
      } else if(t.isDefined){
        val t2 = t.get.get;
        if(len >= t2._3){
          System.arraycopy(t2._1, t2._2, buf, off, t2._3);
          (t2._3, t2._4);
        } else {
          System.arraycopy(t2._1, t2._2, buf, off, len);
          val newStream = new JStreamImpl(p, closable);
          newStream.t = Some(Some((t2._1, t2._2 + len, t2._3 - len, t2._4)));
          (len, newStream);
        }
      } else {
        val l = p(buf, off, len);
        if(l < 0){
          t = Some(None);
          (-1, this);
        } else {
          val b = new Array[A](l);
          System.arraycopy(buf, off, b, 0, l);
          val t2 = (b, 0, l, new JStreamImpl(p, closable));
          t = Some(Some(t2));
          (l, t2._4);
        }
      }
    }

    def close() = closable.close();

  }

  private[hv] def test(): Seq[Option[String]] = {
    import TestLib._;
    import hydrocul.util.StreamUtil._;

    {
      val str = "あいうえお";
      val reader = new jio.StringReader(str);
      val jstream = fromJava(reader);
      val actual = stream2bin(jstream.toJavaC);
      assertEquals(str, actual) :: Nil;
    }

  }

}
