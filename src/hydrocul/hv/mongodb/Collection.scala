package hydrocul.hv.mongodb;

import com.{ mongodb => m }

trait Collection {

  def insert(value: Map[String, Any]);

  def remove();

  def increment(key: String): Long;

  def iterator(): Iterator[Map[String, Any]];

  def head(): Map[String, Any] = iterator().next();

  def seq(): Seq[Map[String, Any]] = iterator().toSeq;

  def filter(key: String, value: Any): Collection = filterEq(key, value);

  def filterEq(key: String, value: Any): Collection;

  def filterLt(key: String, value: Any): Collection;

  def filterLe(key: String, value: Any): Collection;

  def filterGt(key: String, value: Any): Collection;

  def filterGe(key: String, value: Any): Collection;

  def sortWith(key: String): Collection = sortWith(key, false);

  def sortWith(key: String, reverse: Boolean): Collection;

}

private[mongodb] class CollectionImpl(collection: m.DBCollection,
  query: Map[String, Any], sort: Map[String, Any]) extends Collection {

  def this(collection: m.DBCollection) =
    this(collection, Map.empty[String, Any], Map.empty[String, Any]);

  def insert(value: Map[String, Any]){
    collection.insert(mapConvertToJava(value));
  }

  def remove(){
    collection.remove(mapConvertToJava(query));
  }

  def increment(key: String): Long = {
    import scala.collection.JavaConverters._;
    val update = Map("$inc" -> Map(key -> 1));
    val result = collection.findAndModify(mapConvertToJava(query), null, null, false,
      mapConvertToJava(update), true, true);
    if(result == null){
      throw new NoSuchElementException();
    }
    val result2 = mapConvertFromJava(result.asInstanceOf[m.BasicDBObject]).get(key);
    result2 match {
      case Some(result2: Int) => result2.asInstanceOf[Long];
      case Some(result2: Double) => result2.asInstanceOf[Long];
      case _ => throw new Exception("key: %s, result: %s".format(key, result2.toString));
    }
  }

  def iterator(): Iterator[Map[String, Any]] = {
    import scala.collection.JavaConverters._;
    val cursor = collection.find(mapConvertToJava(query));
    val cursor2 = if(sort.isEmpty) cursor.iterator().asScala else
      cursor.sort(mapConvertToJava(sort)).iterator().asScala;
    cursor2.map(t => mapConvertFromJava(t.asInstanceOf[m.BasicDBObject]));
  }

  def filterEq(key: String, value: Any): CollectionImpl = {
    val newQuery = (query.get(key), value) match {
      case (Some(m: Map[_, _]), value: Map[_, _]) if(m.size == 1 && value.size == 1) =>
        val f = (m.head._1, value.head._1) match {
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
          query + (key -> (m.asInstanceOf[Map[String, Any]] + value.head.asInstanceOf[(String, Any)]));
        } else {
          throw new IllegalArgumentException(key);
        }
      case (Some(m), _) =>
        throw new IllegalArgumentException("%s -> %s".format(key, value));
      case (None, _) =>
        query + (key -> value);
    }
    new CollectionImpl(collection, newQuery, sort);
  }

  def filterLt(key: String, value: Any): CollectionImpl =
    filterEq(key, Map("$lt" -> value));

  def filterLe(key: String, value: Any): CollectionImpl =
    filterEq(key, Map("$lte" -> value));

  def filterGt(key: String, value: Any): CollectionImpl =
    filterEq(key, Map("$gt" -> value));

  def filterGe(key: String, value: Any): CollectionImpl =
    filterEq(key, Map("$gte" -> value));

  def sortWith(key: String, reverse: Boolean): Collection = {
    val newSort = if(sort.isDefinedAt(key)){
      throw new IllegalArgumentException(key);
    } else {
      sort + (key -> (if(reverse) -1 else +1));
    }
    new CollectionImpl(collection, query, newSort);
  }

}


