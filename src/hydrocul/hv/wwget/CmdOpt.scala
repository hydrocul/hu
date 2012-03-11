package hydrocul.hv.wwget;

import hydrocul.hv.http.UrlInfo;

/**
 * コマンドラインオプションの情報を保存するクラス。
 */
private[wwget] case class CmdOpt (
  url: String,
  output: CmdOpt.Output,
  selector: Option[CmdOpt.Selector]
){
  override def toString(): String = {
    url; // TODO
  }
}

private[wwget] object CmdOpt {

  def apply(args: List[String]): CmdOpt =
    CmdOptBuilder(None, None, None).parse(args);

  sealed trait Output;

  case class FileOutput (
    fileName: String
  ) extends Output;

  object StdOutput extends Output;

  case class Selector (
    selector: String,
    content: Selector.Content
  );

  object Selector {

    sealed trait Content;

//    object OuterHtml extends Content;

//    object InnerHtml extends Content;

    object Text extends Content;

//    case class Attribute(name: String) extends Content;

  }

}

private[wwget] case class CmdOptBuilder (
  url: Option[String],
  output: Option[CmdOpt.Output],
  selector: Option[CmdOpt.Selector]
){

  def parse(args: List[String]): CmdOpt = {
    (this, args) match {
      case (_, Nil) => build;
      case (CmdOptBuilder(url, None, selector), "-O" :: ofname :: tail) =>
        CmdOptBuilder(url, Some(CmdOpt.FileOutput(ofname)), selector).parse(tail);
      case (CmdOptBuilder(url, None, selector), "--stdout" :: tail) =>
        CmdOptBuilder(url, Some(CmdOpt.StdOutput), selector).parse(tail);
      case (CmdOptBuilder(url, ofname, None), "--selector" :: selector :: tail) =>
        CmdOptBuilder(url, ofname,
          Some(CmdOpt.Selector(selector, CmdOpt.Selector.Text))).parse(tail);
      case (CmdOptBuilder(None, ofname, selector), url :: tail) =>
        CmdOptBuilder(Some(url), ofname, selector).parse(tail);
      case _ => throw new IllegalArgumentException(args.toString);
    }
  }

  private def build: CmdOpt = {
    this match {
      case CmdOptBuilder(Some(url), None, selector) =>
        CmdOpt(url, CmdOpt.FileOutput(CmdOptBuilder.urlToOutputFileName(url)), selector);
      case CmdOptBuilder(Some(url), Some(output), selector) =>
        CmdOpt(url, output, selector);
      case _ => throw new IllegalArgumentException(this.toString);
    }
  }

}

private[wwget] object CmdOptBuilder {

  private def urlToOutputFileName(url: String): String = {

    val UrlPattern1 = ".*/([^/?]*)".r;
    val UrlPattern2 = ".*/([^/?]*)?(.*)".r;

    UrlInfo(url).path match {
      case "" => "index.html";
      case UrlPattern1("") => "index.html";
      case UrlPattern1(fname) => fname;
      case UrlPattern2("", _) => "index.html";
      case UrlPattern2(fname, _) => fname;
      case path => throw new AssertionError(path);
    }

  }

}
