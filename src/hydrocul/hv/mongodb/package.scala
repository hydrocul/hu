package hydrocul.hv;

import com.{ mongodb => m }
import org.bson.{ types => mtypes }

package object mongodb {

  implicit def convertObjectIdFromJava(value: mtypes.ObjectId): ObjectIdDBObject =
    ObjectIdDBObject(value);

  implicit def convertObjectIdToJava(obj: ObjectIdDBObject): mtypes.ObjectId =
    obj.toJava;

  implicit def convertCodeFromJava(value: mtypes.Code): CodeDBObject =
    CodeDBObject(value);

  implicit def convertCodeToJava(obj: CodeDBObject): mtypes.Code =
    obj.toJava;

  implicit def convertStringFromJava(value: String): StringDBObject =
    StringDBObject(value);

  implicit def convertStringToJava(obj: StringDBObject): String =
    obj.toJava;

  implicit def convertIntFromJava(value: Int): DoubleDBObject =
    DoubleDBObject(value);

  implicit def convertDoubleFromJava(value: Double): DoubleDBObject =
    DoubleDBObject(value);

  implicit def convertDoubleToJava(obj: DoubleDBObject): java.lang.Double =
    obj.toJava;

  implicit def convertMapFromJava(value: m.BasicDBObject): MapDBObject =
    MapDBObject(value);

  implicit def convertMapFromJava(value: Map[String, Any]): MapDBObject =
    MapDBObject(value.map(t => (t._1, DBObject.convertFromJava(t._2))));

  implicit def convertMapToJava(obj: MapDBObject): m.BasicDBObject =
    obj.toJava;

}

