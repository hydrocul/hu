package hydrocul.hv.mongodb;

import com.{ mongodb => m }

trait Collection {

  def insert(value: MapDBObject);

  def remove();

  def iterator(): Iterator[Map[String, DBObject]];

  def filterEq(key: String, value: DBObject): Collection;

  def filterLt(key: String, value: DBObject): Collection;

}

private[mongodb] class CollectionImpl(collection: m.DBCollection,
  ref: MapDBObject) extends Collection {

  def this(collection: m.DBCollection) =
    this(collection, MapDBObject(Map.empty[String, DBObject]));

  def insert(value: MapDBObject){
    collection.insert(value.toJava);
  }

  def remove(){
    collection.remove(ref.toJava);
  }

  def iterator(): Iterator[Map[String, DBObject]] = {
    import scala.collection.JavaConverters._;
    val cursor = collection.find(ref.toJava).iterator().asScala;
    cursor.map(_.asInstanceOf[m.BasicDBObject].asScala.
      map(t => (t._1, DBObject.convertFromJava(t._2))).toMap);
  }

  def filterEq(key: String, value: DBObject): CollectionImpl = {
    if(ref.value.contains(key)){
      throw new IllegalArgumentException(key);
    }
    new CollectionImpl(collection, MapDBObject(ref.value + (key -> value)));
  }

  def filterLt(key: String, value: DBObject): CollectionImpl =
    filterEq(key, Map("$lt" -> value));

}


