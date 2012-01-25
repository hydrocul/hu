package hydrocul.hv;

import com.{ mongodb => m }
import org.bson.{ types => mtypes }

package object mongodb {

  def convertToJava(value: Any): Any = DBObject.convertToJava(value);

  def mapConvertToJava(value: Map[String, _]): m.BasicDBObject = DBObject.mapConvertToJava(value);

  def convertFromJava(value: Any): Any = DBObject.convertFromJava(value);

  def mapConvertFromJava(value: m.BasicDBObject): Map[String, Any] = DBObject.mapConvertFromJava(value);

}

