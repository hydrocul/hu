package hydrocul.hv.mongodb;

import com.{ mongodb => m }

object Mongo {

  def apply(dbname: String): Database = apply("localhost", 27017, dbname);

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
    val collection = Mongo("localhost", 27017, "hutest").collection("testcollection");
    collection.remove();
    val count0 = collection.seq().size; // 0
    collection.insert(Map("a" -> 1, "b" -> 2, "_id" -> 1));
    val count1 = collection.seq().size; // 1
    collection.insert(Map("a" -> 1, "b" -> "abc", "_id" -> 2));
    collection.insert(Map("a" -> 1, "b" -> "def", "_id" -> 3));
    val count2 = collection.seq().size; // 3
    val value1 = collection.filterEq("b", "abc").head()("a"); // 1.0
    val count3 = collection.filterEq("b", "ABC").seq().size; // 0
    val count4 = collection.filterLt("_id", 3).seq().size; // 2
    val count5 = collection.filterLe("_id", 3).seq().size; // 3
    val count6 = collection.filterLe("_id", 3).filterGe("_id", 2).seq().size; // 2
    val value2 = collection.filterEq("b", "abc").increment("a"); // 2
    val value3 = collection.filterEq("b", "abc").increment("a"); // 3
    val value4 = collection.sortWith("_id", false).head()("b"); // 2
    val value5 = collection.sortWith("_id", true).head()("b"); // "def"
    collection.remove();
    val count7 = collection.iterator().toSeq.size; // 0
    List(
      assertEquals(0, count0),
      assertEquals(1, count1),
      assertEquals(3, count2),
      assertEquals(1.0, value1),
      assertEquals(0, count3),
      assertEquals(2, count4),
      assertEquals(3, count5),
      assertEquals(2, count6),
      assertEquals(2L, value2),
      assertEquals(3L, value3),
      assertEquals(2, value4),
      assertEquals("def", value5),
      assertEquals(0, count7)
    );
  }

}


