package hydrocul.hv.http;

abstract class Page private[http] (val _response: Response, _url: String){

  def response: Response = _response;

  def url: String = _url;

  def toString: String;

}

