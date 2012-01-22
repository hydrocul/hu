package hydrocul.hv.mongodb;

import com.{ mongodb => m }

trait Database {

  def collection(name: String): Collection;

  def collectionNames: Set[String];

}

private[mongodb] class DatabaseImpl(db: m.DB) extends Database {

  def collection(name: String): Collection = {
    val coll = db.getCollection(name);
    new CollectionImpl(coll);
  }

  def collectionNames: Set[String] = {
    import scala.collection.JavaConverters._;
    db.getCollectionNames.asScala.toSet;
  }

}


