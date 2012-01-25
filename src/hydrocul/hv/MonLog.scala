package hydrocul.hv;

class MonLog private(db: mongodb.Database, yisp: Yisp) {

  def append(data: Map[String, Any]){
    val counter = db.collection("misc").filter("name", "counter").increment("value");
    val now = new java.util.Date();
    val timestamp = now.getTime; // the number of milliseconds since January 1, 1970, 00:00:00 GMT
    val timestr = "%1$tY-%1$tm-%1$tdT%1$tH:%1$tM:%1$tS.%1$tL".format(now);
    val data2 = data + ("_id" -> counter) + ("timestamp" -> timestamp) + ("timestr" -> timestr);
    collection.insert(data2);
  }

  def putFile(bin: Array[Byte]): String = yisp.put(bin);

  def getFile(key: String): Array[Byte] = yisp.get(key);

  def iterator(): Iterator[Map[String, Any]] = selector.iterator();

  def head(): Map[String, Any] = iterator().next();

  def seq(): Seq[Map[String, Any]] = iterator().toSeq;

  def filter(key: String, value: Any): MonLog.Selector = filterEq(key, value);

  def filterEq(key: String, value: Any): MonLog.Selector = selector.filterEq(key, value);

  def filterLt(key: String, value: Any): MonLog.Selector = selector.filterLt(key, value);

  def filterLe(key: String, value: Any): MonLog.Selector = selector.filterLe(key, value);

  def filterGt(key: String, value: Any): MonLog.Selector = selector.filterGt(key, value);

  def filterGe(key: String, value: Any): MonLog.Selector = selector.filterGe(key, value);

  def sortWith(key: String, reverse: Boolean): MonLog.Selector = selector.sortWith(key, reverse);

  private lazy val collection = db.collection("log");

  private lazy val selector = new MonLog.SelectorImpl(collection);

}

object MonLog {

  def apply(db: mongodb.Database, yisp: Yisp): MonLog = new MonLog(db, yisp);

  trait Selector {

    def remove();

    def iterator(): Iterator[Map[String, Any]];

    def head(): Map[String, Any] = iterator().next();

    def seq(): Seq[Map[String, Any]] = iterator().toSeq;

    def filter(key: String, value: Any): Selector = filterEq(key, value);

    def filterEq(key: String, value: Any): Selector;

    def filterLt(key: String, value: Any): Selector;

    def filterLe(key: String, value: Any): Selector;

    def filterGt(key: String, value: Any): Selector;

    def filterGe(key: String, value: Any): Selector;

    def sortWith(key: String): Selector = sortWith(key, false);

    def sortWith(key: String, reverse: Boolean): Selector;

  }

  private class SelectorImpl(collection: mongodb.Collection) extends Selector {

    def remove() = collection.remove();

    def iterator(): Iterator[Map[String, Any]] = collection.iterator();

    def filterEq(key: String, value: Any) = new SelectorImpl(collection.filterEq(key, value));

    def filterLt(key: String, value: Any) = new SelectorImpl(collection.filterLt(key, value));

    def filterLe(key: String, value: Any) = new SelectorImpl(collection.filterLe(key, value));

    def filterGt(key: String, value: Any) = new SelectorImpl(collection.filterGt(key, value));

    def filterGe(key: String, value: Any) = new SelectorImpl(collection.filterGe(key, value));

    def sortWith(key: String, reverse: Boolean) = new SelectorImpl(collection.sortWith(key, reverse));

  }

}

