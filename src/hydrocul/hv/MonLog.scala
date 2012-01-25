package hydrocul.hv;

class MonLog private(db: mongodb.Database, yisp: Yisp){

  def append(data: Map[String, Any]){
    val counter = db.collection("misc").filter("name", "counter").increment("value");
    val now = new java.util.Date();
    val timestamp = now.getTime; // the number of milliseconds since January 1, 1970, 00:00:00 GMT
    val timestr = "%1$tY-%1$tm-%1$tdT%1$tH:%1$tM:%1$tS.%1$tL".format(now);
    val data2 = data + ("_id" -> counter) + ("timestamp" -> timestamp) + ("timestr" -> timestr);
    db.collection("log").insert(data2);
  }

}

object MonLog {

  def apply(db: mongodb.Database, yisp: Yisp): MonLog = new MonLog(db, yisp);

}

