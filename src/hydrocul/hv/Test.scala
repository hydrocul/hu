package hydrocul.hv;

object Test {

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
