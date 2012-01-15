package hydrocul.hv.http;

import hydrocul.hv.XmlElement;

trait HtmlElement {

  def select(query: String): IndexedSeq[HtmlElement];

  def outerHtml: String;

  def html: String;

  def text: String;

  def attr(name: String): String;

  override def toString(): String = outerHtml;

}

private[http] class HtmlElementImpl private (elem: XmlElement) extends HtmlElement {

  def select(query: String): IndexedSeq[HtmlElement] =
    elem.select(query).map(HtmlElementImpl.create(_));

  def outerHtml: String = elem.outerHtml;

  def html: String = elem.html;

  def text: String = elem.text;

  def attr(name: String): String = elem.attr(name);

}

private[http] object HtmlElementImpl {

  def create(elem: XmlElement): HtmlElement = new HtmlElementImpl(elem);

}

