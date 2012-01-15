package hydrocul.hv;

import scala.collection.IndexedSeqLike;
import scala.collection.mutable.Builder;
import scala.collection.mutable.LazyBuilder

import org.jsoup;
  // http://jsoup.org/apidocs/

trait XmlElement {

  def select(query: String): XmlElements;

  def outerHtml: String;

  def html: String;

  def text: String;

  def attr(name: String): String;

  override def toString(): String = outerHtml;

}

trait XmlElements extends IndexedSeq[XmlElement] {

  def select(query: String): hydrocul.hv.XmlElements;

}

object XmlElement {

  def parseHtml(html: String): XmlElement =
    new XmlElementImpl(jsoup.parser.Parser.parse(html, "").body);

}

private[hv] class XmlElementImpl(val elem: jsoup.nodes.Element) extends XmlElement {

  def select(query: String): XmlElements = {
    new XmlElementsImpl(elem.select(query));
  }

  def outerHtml: String = elem.outerHtml;

  def html: String = elem.html;

  def text: String = elem.text;

  def attr(name: String): String = elem.attr(name);

}

private[hv] class XmlElementsImpl(elems: jsoup.select.Elements) extends XmlElements
    with IndexedSeq[XmlElementImpl] with IndexedSeqLike[XmlElementImpl, XmlElementsImpl] {

  def select(query: String): hydrocul.hv.XmlElements =
    new XmlElementsImpl(elems.select(query));

  def apply(index: Int): XmlElementImpl = new XmlElementImpl(elems.get(index));

  def length: Int = elems.size;

  override def seq = this;

  override protected[this] def newBuilder: Builder[XmlElementImpl, XmlElementsImpl] = {
    import scala.collection.JavaConverters._;
    new LazyBuilder[XmlElementImpl, XmlElementsImpl]{
      override def result: XmlElementsImpl = {
        new XmlElementsImpl(new jsoup.select.Elements(parts.toIterable.
          flatMap(_.toIterable).toBuffer.map { e: XmlElementImpl => e.elem; }.asJava));
      }
    }
  }

}
