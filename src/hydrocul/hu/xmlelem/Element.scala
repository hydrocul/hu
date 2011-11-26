package hydrocul.hu.xmlelem;

import scala.collection.IndexedSeqLike;
import scala.collection.mutable.Builder;
import scala.collection.mutable.LazyBuilder

import org.jsoup;

trait Element {

  def select(query: String): Elements;

  def html: String;

  def text: String;

  def attr(name: String): String;

  override def toString(): String = html;

}

trait Elements extends IndexedSeq[Element] {

  def select(query: String): hydrocul.hu.xmlelem.Elements;

}

object Element {

  def parseHtml(html: String): Element =
    new ElementImpl(jsoup.parser.Parser.parse(html, "").body);

}

private[xmlelem] class ElementImpl(val elem: jsoup.nodes.Element) extends Element {

  def select(query: String): Elements = {
    new ElementsImpl(elem.select(query));
  }

  def html: String = elem.html;

  def text: String = elem.text;

  def attr(name: String): String = elem.attr(name);

}

private[xmlelem] class ElementsImpl(elems: jsoup.select.Elements) extends Elements
    with IndexedSeq[ElementImpl] with IndexedSeqLike[ElementImpl, ElementsImpl] {

  def select(query: String): hydrocul.hu.xmlelem.Elements =
    new ElementsImpl(elems.select(query));

  def apply(index: Int): ElementImpl = new ElementImpl(elems.get(index));

  def length: Int = elems.size;

  override def seq = this;

  override protected[this] def newBuilder: Builder[ElementImpl, ElementsImpl] = {
    import scala.collection.JavaConverters._;
    new LazyBuilder[ElementImpl, ElementsImpl]{
      override def result: ElementsImpl = {
        new ElementsImpl(new jsoup.select.Elements(parts.toIterable.
          flatMap(_.toIterable).toBuffer.map { e: ElementImpl => e.elem; }.asJava));
      }
    }
  }

}
