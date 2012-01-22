package hydrocul.hv.mongodb;

import com.{ mongodb => m }

object Mongo {

  def apply(host: String, port: Int, dbname: String): Database = {
    val mongo = new m.Mongo(host, port);
    val db = mongo.getDB(dbname);
    new DatabaseImpl(db);
  }

  private[hv] def test(): Seq[(String, Function0[Seq[Option[String]]])] = {
    List(
      ("mongodb", testSub)
    );
  }

  private def testSub(): Seq[Option[String]] = {
    import hydrocul.hv.TestLib._;
    val expected = "test";
    val actual = "test";
    List(
      assertEquals(expected, actual)
    );
  }

}


