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
    val collection = Mongo("localhost", 27017, "hutest").collection("testcollection");
    collection.remove();
    val count0 = collection.iterator().toSeq.size; // 0
    collection.insert(Map("a" -> 1, "b" -> 2, "_id" -> 1));
    val count1 = collection.iterator().toSeq.size; // 1
    collection.insert(Map("a" -> 1, "b" -> "abc", "_id" -> 2));
    collection.insert(Map("a" -> 1, "b" -> "def", "_id" -> 3));
    val count2 = collection.iterator().toSeq.size; // 3
    val value1 = collection.filterEq("b", "abc").iterator.next().
      asInstanceOf[Map[String, DBObject]]("a").toJava; // 1.0
    val count3 = collection.filterEq("b", "ABC").iterator.toSeq.size; // 0
    val count4 = collection.filterLt("_id", 3).iterator.toSeq.size; // 2
    collection.remove();
    val count5 = collection.iterator().toSeq.size; // 0
    List(
      assertEquals(0, count0),
      assertEquals(1, count1),
      assertEquals(3, count2),
      assertEquals(1.0, value1),
      assertEquals(0, count3),
      assertEquals(2, count4),
      assertEquals(0, count5)
    );
  }

}


