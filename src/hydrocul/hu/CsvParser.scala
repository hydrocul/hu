package hydrocul.hu;

import java.{ io => jio }

import au.com.bytecode.opencsv.CSVReader;

object CsvParser {

  def parse(source: String): Seq[Seq[String]] = {
    import scala.collection.JavaConverters._;
    val reader = new CSVReader(new jio.StringReader(source));
    val result = reader.readAll;
    result.asScala.toSeq.map(_.toSeq);
  }

  def test: Seq[Option[String]] = {

    import hydrocul.hu.TestLib._;

    def testParse(source: String, expected: Any): Option[String] = {
      assertEquals(expected, parse(source));
    }

    List(
      testParse("abc,124", List("abc" :: "124" :: Nil)),
      testParse("abc,\"124\n567\",ABC\n123", List(
        "abc" :: "124\n567" :: "ABC" :: Nil,
        "123" :: Nil
      )),
      testParse("abc,\"12\"\"4\n567\",ABC\n123", List(
        "abc" :: "12\"4\n567" :: "ABC" :: Nil,
        "123" :: Nil
      ))
    );

  }

}
