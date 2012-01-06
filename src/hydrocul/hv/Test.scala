package hydrocul.hv;

object Test {

  def test(all: Boolean): Seq[Function0[Seq[Option[String]]]] = {
    List(
      testTest,
      CipherUtil.test,
      EncodingMania.test
    )
  }

  private def testTest(): Seq[Option[String]] = {
    // hvパッケージでtestが動いていることのテスト
    import TestLib._;
    assertEquals("test", "test") :: Nil;
  }

}
