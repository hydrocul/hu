package hydrocul.util;

import org.w3c.dom;

import scala.util.matching.Regex;
import scala.xml;

object ScalaUtil {

  def using[A <: { def close(); }, B](resource: A)(p: A=>B): B = {
    try {
      p(resource);
    } finally {
      resource.close();
    }
  }

  def returning[A](obj: A)(p: A=>Unit): A = {
    p(obj);
    obj;
  }

  def block2runnable(p: =>Unit): Runnable = new Runnable(){
    override def run(){
      p;
    }
  }

}

