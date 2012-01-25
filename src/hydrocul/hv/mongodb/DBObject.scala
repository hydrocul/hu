package hydrocul.hv.mongodb;

import com.{ mongodb => m }
import org.bson.{ types => mtypes }

import hydrocul.hv.EncodingMania;

private[mongodb] object DBObject {

  def convertToJava(value: Any): Any = {
    value match {
      case value: ObjectId => value.toJava;
      case value: Code => value.toJava;
      case value: String => value;
      case value: Int => value;
      case value: Double => value;
      case value: Map[_, _] =>
        if(value.exists(t => !t._1.isInstanceOf[String])){
          throw new IllegalArgumentException("%s".format(value.toString));
        }
        mapConvertToJava(value.asInstanceOf[Map[String, Any]]);
      case value: AnyRef =>
        throw new IllegalArgumentException("%s: %s".format(
          value.getClass, value.toString));
      case _ =>
        throw new IllegalArgumentException("%s".format(value.toString));
    }
  }

  def mapConvertToJava(value: Map[String, Any]): m.BasicDBObject = {
    import scala.collection.JavaConverters._;
    new m.BasicDBObject(value.map(t => (t._1, convertToJava(t._2))).asJava);
  }

  def convertFromJava(value: Any): Any = {
    value match {
      case value: mtypes.ObjectId =>
        new ObjectId(value.toByteArray);
      case value: mtypes.Code =>
        new Code(value.getCode);
      case value: String => value;
      case value: Int => value;
      case value: Double => value;
      case value: m.BasicDBObject =>
        mapConvertFromJava(value);
      case value: AnyRef =>
        throw new IllegalArgumentException("%s: %s".format(
          value.getClass, value.toString));
      case _ =>
        throw new IllegalArgumentException("%s".format(value.toString));
    }
  }

  def mapConvertFromJava(value: m.BasicDBObject): Map[String, Any] = {
    import scala.collection.JavaConverters._;
    value.asScala.toMap.map(t => (t._1, convertFromJava(t._2)));
  }

}

case class ObjectId(value: Array[Byte]){

  override def toString(): String =
    "ObjectId(" + EncodingMania.encodeHex(value) + ")";

  def toJava: mtypes.ObjectId = new mtypes.ObjectId(value);

}

case class Code(value: String){

  override def toString(): String =
    "Code(" + value + ")";

  def toJava: mtypes.Code = new mtypes.Code(value);

}


