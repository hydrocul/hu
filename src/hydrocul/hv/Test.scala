package hydrocul.hv;

import hydrocul.util.StreamUtil;

object Test {

  def main(args: Array[String]){
    main(args.toList);
  }

  private def main(args: List[String]){
    args match {
      case "test" :: "all" :: Nil =>
        doTest(true);
      case "test" :: Nil =>
        doTest(false);
      case _ =>
        throw new IllegalArgumentException();
    }
  }

  private def doTest(all: Boolean){
    val result: Seq[(Int, Int)] = testTask(all).par.map { t =>
      val result: Seq[Option[String]] = try {
        t._2.apply();
      } catch { case e =>
        Some(StreamUtil.exception2stackTrace(e).trim) :: Nil;
      }
      val failed = result.filter(_.isDefined).size;
      val success = result.size - failed;
      val resultMsg = "%d / %d: %s".format(success, success + failed, t._1) + (
        result.map {
          case Some(failedMsg) => "\n" + failedMsg;
          case _ => "";
        }.mkString("")
      );
      println(resultMsg);
      (success, failed);
    }.seq;
    val success = result.map(_._1).sum;
    val failed = result.map(_._2).sum;
    if(failed > 0){
      println("Failed: %d / %d".format(success, success + failed));
      sys.exit(1);
    } else {
      println("Success: %d / %d".format(success, success + failed));
      sys.exit(0);
    }
  }

  private def testTask(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    {
      val huTest = hydrocul.hu.IO.createTestFunc(all);
      (0 until huTest.size).map(i => ("hu.IO (%d)".format(i + 1), huTest(i)));
    } ++ test(all);
  }

  def test(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    http.WebBrowser.test(all) ++
    rosen.test(all) ++
    // mongodb.Mongo.test() ++
    List[(String, Function0[Seq[Option[String]]])](
      ("Test", testTest),
      ("CipherUtil", CipherUtil.test),
      ("DateStringUtil", DateStringUtil.test),
      ("EncodingMania", EncodingMania.test),
      ("Json", Json.test),
      ("JStream", JStream.test),
      ("StreamUtil2", StreamUtil2.test),
      XmlElement.test()
    );
  }

  private def testTest(): Seq[Option[String]] = {
    // hvパッケージでtestが動いていることのテスト
    import TestLib._;
    assertEquals("test", "test") :: Nil;
  }

}
