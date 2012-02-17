package hydrocul.hv.http;

import scala.collection.IndexedSeqLike;
import scala.collection.mutable.Builder;
import scala.collection.mutable.LazyBuilder

import hydrocul.hv.XmlElement;

trait HtmlElement {

  def select(query: String): hydrocul.hv.http.HtmlElements;

  def outerHtml: String;

  def html: String;

  def text: String;

  def attr(name: String): String;

  override def toString(): String = outerHtml;

}

trait HtmlElements extends IndexedSeq[HtmlElement] {

  def select(query: String): hydrocul.hv.http.HtmlElements =
    apply(0).select(query);

  def outerHtml: String = apply(0).outerHtml;

  def html: String = apply(0).html;

  def text: String = apply(0).text;

  def attr(name: String): String = apply(0).attr(name);

}

private[http] class HtmlElementImpl(val elem: XmlElement) extends HtmlElement {

  def select(query: String): HtmlElements =
    new HtmlElementsImpl(elem.select(query).map(e => new HtmlElementImpl(e)));

  def outerHtml: String = elem.outerHtml;

  def html: String = elem.html;

  def text: String = elem.text;

  def attr(name: String): String = elem.attr(name);

}

private[http] class HtmlElementsImpl(elems: IndexedSeq[HtmlElementImpl]) extends HtmlElements
    with IndexedSeq[HtmlElementImpl] with IndexedSeqLike[HtmlElementImpl, HtmlElementsImpl] {

  def apply(index: Int): HtmlElementImpl = elems(index);

  def length: Int = elems.size;

  override def seq = this;

  override protected[this] def newBuilder: Builder[HtmlElementImpl, HtmlElementsImpl] = {
    import scala.collection.JavaConverters._;
    new LazyBuilder[HtmlElementImpl, HtmlElementsImpl]{
      override def result: HtmlElementsImpl = {
        new HtmlElementsImpl(parts.toIterable.
          flatMap(_.toIterable).toIndexedSeq);
//          flatMap(_.toIterable).toBuffer.map { e: HtmlElementImpl => e.elem; });
      }
    }
  }

}
