package hydrocul.hv;

package object rosen {

  private[hv] def test(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    ekikara.EkikaraTableScraper.test(all);
  }

}
