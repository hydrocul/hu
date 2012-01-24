package hydrocul.hv.mongodb;

import com.{ mongodb => m }
import org.bson.{ types => mtypes }

import hydrocul.hv.EncodingMania;

trait DBObject {

  def toJava: Any;

}

private[mongodb] object DBObject {

  def convertFromJava(value: Any): DBObject = {
    value match {
      case value: DBObject => value;
      case value: mtypes.ObjectId => ObjectIdDBObject(value);
      case value: mtypes.Code => CodeDBObject(value);
      case value: String => StringDBObject(value);
      case value: Int => DoubleDBObject(value.asInstanceOf[Double]);
//      case value: java.lang.Integer => DoubleDBObject(value.asInstanceOf[Int].asInstanceOf[Double]);
//      case value: java.lang.Long => DoubleDBObject(value: Double);
//      case value: java.lang.Float => DoubleDBObject(value: Double);
      case value: java.lang.Double => DoubleDBObject(value);
      case value: m.BasicDBObject => MapDBObject(value);
      case value: Map[String, Any] => convertMapFromJava(value);
      case value: AnyRef => throw new IllegalArgumentException("%s: %s".format(
        value.getClass, value.toString));
      case _ => throw new IllegalArgumentException("%s".format(value.toString));
    }
  }

}

case class ObjectIdDBObject(value: Array[Byte]) extends DBObject {

  override def toString(): String =
    "ObjectIdDBObject(" + EncodingMania.encodeHex(value) + ")";

  def toJava: mtypes.ObjectId = new mtypes.ObjectId(value);

}

object ObjectIdDBObject {

  def apply(value: mtypes.ObjectId) = new ObjectIdDBObject(value.toByteArray);

}

case class CodeDBObject(value: String) extends DBObject {

  def toJava: mtypes.Code = new mtypes.Code(value);

}

object CodeDBObject {

  def apply(value: mtypes.Code) = new CodeDBObject(value.getCode);

}

case class StringDBObject(value: String) extends DBObject {

  override def toString(): String = value;

  def toJava: String = value;

}

object StringDBObject {

}

case class DoubleDBObject(value: Double) extends DBObject {

  override def toString(): String = value.toString;

  def toJava: Double = value;

}

case class MapDBObject(value: Map[String, DBObject]) extends DBObject {

  override def toString(): String = value.toString;

  def toJava: m.BasicDBObject = {
    import scala.collection.JavaConverters._;
    new m.BasicDBObject(value.map(t => (t._1, t._2.toJava)).asJava);
  }

}

object MapDBObject {

  def apply(value: m.BasicDBObject) = {
    import scala.collection.JavaConverters._;
    new MapDBObject(value.asScala.toMap.map(t => (t._1, DBObject.convertFromJava(t._2))));
  }

}



