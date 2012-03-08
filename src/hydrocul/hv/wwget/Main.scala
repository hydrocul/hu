package hydrocul.hv.wwget;

import hydrocul.hv.http.WebBrowser;

object Main {

  def main(args: Array[String]){
    val cmdOpt = CmdOpt(args.toList);
    val browser = WebBrowser.create();
    val page = browser.doGet(cmdOpt.url);
    System.out.write(page.response.body);
  }

}
