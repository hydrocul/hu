package hydrocul.hv.mongodb;

import com.{ mongodb => m }

trait Collection {

  def insert(value: MapDBObject);

  def remove();

  def increment(key: String): Long;

  def iterator(): Iterator[Map[String, DBObject]];

  def filterEq(key: String, value: DBObject): Collection;

  def filterLt(key: String, value: DBObject): Collection;

  def filterLe(key: String, value: DBObject): Collection;

  def filterGt(key: String, value: DBObject): Collection;

  def filterGe(key: String, value: DBObject): Collection;

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

  def increment(key: String): Long = {
    import scala.collection.JavaConverters._;
    val update: MapDBObject = Map("$inc" -> Map(key -> 1));
    val result = collection.findAndModify(ref.toJava, null, null, false, update.toJava, true, true);
    val result2 = result.asInstanceOf[m.BasicDBObject].asScala.get(key).get;
    result2 match {
      case result2: java.lang.Double => result2.asInstanceOf[Double].asInstanceOf[Long];
      case _ => throw new Exception("key: %s, result: %s".format(key, result2.toString));
    }
  }

  def iterator(): Iterator[Map[String, DBObject]] = {
    import scala.collection.JavaConverters._;
    val cursor = collection.find(ref.toJava).iterator().asScala;
    cursor.map(_.asInstanceOf[m.BasicDBObject].asScala.
      map(t => (t._1, DBObject.convertFromJava(t._2))).toMap);
  }

  def filterEq(key: String, value: DBObject): CollectionImpl = {
    val newRef = (ref.value.get(key), value) match {
      case (Some(m: MapDBObject), value: MapDBObject) if(m.value.size == 1 && value.value.size == 1) =>
        val f = (m.value.head._1, value.value.head._1) match {
          case ("$lt", "$gt") => true;
          case ("$lt", "$gte") => true;
          case ("$lte", "$gt") => true;
          case ("$lte", "$gte") => true;
          case ("$gt", "$lt") => true;
          case ("$gt", "$lte") => true;
          case ("$gte", "$lt") => true;
          case ("$gte", "$lte") => true;
          case _ => false;
        }
        if(f){
          ref.value + (key -> MapDBObject(m.value + value.value.head));
        } else {
          throw new IllegalArgumentException(key);
        }
      case (Some(m), _) =>
        throw new IllegalArgumentException("%s -> %s".format(key, value));
      case (None, _) =>
        ref.value + (key -> value);
    }
    new CollectionImpl(collection, MapDBObject(newRef));
  }

  def filterLt(key: String, value: DBObject): CollectionImpl =
    filterEq(key, Map("$lt" -> value));

  def filterLe(key: String, value: DBObject): CollectionImpl =
    filterEq(key, Map("$lte" -> value));

  def filterGt(key: String, value: DBObject): CollectionImpl =
    filterEq(key, Map("$gt" -> value));

  def filterGe(key: String, value: DBObject): CollectionImpl =
    filterEq(key, Map("$gte" -> value));

}


