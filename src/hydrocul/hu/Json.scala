package hydrocul.hu;

import net.arnx.jsonic;

object Json {

  def decode(source: String): Any = {
    val jsonicResult = jsonic.JSON.decode[Any](source match {
      case JsonpPattern(s) => s;
      case s => s;
    });
    decodeObj(jsonicResult);
  }

  private val JsonpPattern = "(?s)[\\uFEFF|\\uFFFE]?\\s*[a-zA-Z_][a-zA-Z0-9_]*\\s*\\((.*)\\)\\s*;?\\s*".r;

  private def decodeObj(json: Any): Any = {
    import scala.collection.JavaConverters._;
    json match {
      case json: java.util.LinkedHashMap[_, _] =>
        (json.asScala.toMap: Map[_, _]).mapValues(decodeObj(_));
      case json: java.util.ArrayList[_] =>
        (json.asScala.toIndexedSeq: IndexedSeq[_]).map(decodeObj(_));
      case json: String =>
        json;
      case json: java.math.BigDecimal =>
        scala.math.BigDecimal(json);
      case json: Boolean =>
        json;
      case null =>
        None;
      case _ =>
        throw new IllegalArgumentException(json.toString);
    }
  }

/*
  private[hu] def test(io: TestIO){ // TODO
    io.putTestTask(2){ io =>
      val expected = Map(
        "a" -> "abc",
        "b" -> Vector(BigDecimal(123), BigDecimal(456)),
        "c" -> true,
        "d" -> false,
        "e" -> None);
      io.assertEquals(expected,
        decode("{a:\"abc\", b:[123, 456], c:true, d:false, e:null}"));
      io.assertEquals(expected,
        decode("callback({a:\"abc\", b:[123, 456], c:true, d:false, e:null})"));
    }
  }
*/

}
