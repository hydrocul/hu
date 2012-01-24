package hydrocul.hv;

class MonLog private(db: mongodb.Database, yisp: Yisp){

  def append(data: Map[String, Any]){
    val counter = db.collection("misc").filter("name", "counter").increment("value");
    val data2 = data + ("_id" -> counter);
    db.collection("log").insert(data2);
  }

}

object MonLog {

  def apply(db: mongodb.Database, yisp: Yisp): MonLog = new MonLog(db, yisp);

}

