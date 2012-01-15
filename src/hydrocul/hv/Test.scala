package hydrocul.hv;

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
      println("start %s".format(t._1));
      val result: Seq[Option[String]] = t._2.apply();
      val failed = result.filter(_.isDefined).size;
      val success = result.size - failed;
      val resultMsg = "%s %d / %d".format(t._1, success, success + failed) + (
        result.map {
          case Some(failedMsg) => failedMsg + "\n";
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
    val huTest = hydrocul.hu.IO.createTestFunc(all);
    (0 until huTest.size).map(i => ("hu.IO (%d)".format(i + 1), huTest(i))) ++
    Nil;
  }

  def test(all: Boolean): Seq[Function0[Seq[Option[String]]]] = {
    List(
      testTest,
      CipherUtil.test,
      EncodingMania.test,
      Json.test,
      JStream.test,
      StreamUtil2.test,
      http.WebBrowser.test
    )
  }

  private def testTest(): Seq[Option[String]] = {
    // hvパッケージでtestが動いていることのテスト
    import TestLib._;
    assertEquals("test", "test") :: Nil;
  }

}
