package hydrocul.hv.wwget;

import hydrocul.hv.File;
import hydrocul.hv.http.WebBrowser;
import hydrocul.hv.http.UrlInfo;

object Main {

  def main(args: Array[String]){
    val cmdOpt = CmdOpt(args.toList);
    exec(cmdOpt);
  }

  def exec(cmdOpt: CmdOpt){

    val fname = outputFileName(cmdOpt);

    val file = File(fname);
    if(file.exists){
      throw new IllegalStateException("File already exists: " + fname);
    }

    val browser = WebBrowser.create();
    val page = browser.doGet(cmdOpt.url);
    val body = page.response.body;

    file.write(Some(body));

  }

  private def outputFileName(cmdOpt: CmdOpt): String = {

    val UrlPattern1 = ".*/([^/?]*)".r;
    val UrlPattern2 = ".*/([^/?]*)?(.*)".r;

    UrlInfo(cmdOpt.url).path match {
      case "" => "index.html";
      case UrlPattern1("") => "index.html";
      case UrlPattern1(fname) => fname;
      case UrlPattern2("", _) => "index.html";
      case UrlPattern2(fname, _) => fname;
      case path => throw new AssertionError(path);
    }

  }

}
