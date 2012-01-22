package hydrocul.hv.mongodb;

import com.{ mongodb => m }

trait Collection {

/*
  def all(): Iterator[Map[String, DBObject]];
*/

}

private[mongodb] class CollectionImpl(collection: m.DBCollection) extends Collection {

/*
  def all(): Iterator[Map[String, DBObject]] = {
    import scala.collection.JavaConverters._;
    val cursor = collection.find().iterator().asScala;
    cursor.map(_.asInstanceOf[m.BasicDBObject].asScala.toMap);
  }
*/

}


