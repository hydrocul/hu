package hydrocul;

package object hv {

  import hydrocul.hu;

  val CipherUtil = hu.CipherUtil;

  val EncodingMania = hu.EncodingMania;

  val TestLib = hu.TestLib;

  private[hv] def packageTest: () => Seq[Option[String]] = { () =>
    Nil;
  }

}
