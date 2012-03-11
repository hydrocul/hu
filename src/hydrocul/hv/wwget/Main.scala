package hydrocul.hv.wwget;

import hydrocul.hv.EncodingMania;
import hydrocul.hv.File;
import hydrocul.hv.http.HtmlPage;
import hydrocul.hv.http.WebBrowser;

object Main {

  def main(args: Array[String]){
    val argsList = args.toList;
    if(argsList == "--help" :: Nil){
      help();
    } else {
      val cmdOpt = CmdOpt(argsList);
      exec(cmdOpt);
    }
  }

  def exec(cmdOpt: CmdOpt){

    val output: Array[Byte] => Unit = cmdOpt.output match {
      case CmdOpt.FileOutput(fname) =>
        val file = File(fname);
        if(file.exists){
          throw new IllegalStateException("File already exists: " + fname);
        }
        { bin: Array[Byte] =>
          file.write(Some(bin));
        }
      case CmdOpt.StdOutput =>
        { bin: Array[Byte] =>
          System.out.write(bin);
        }
    }

    val browser = WebBrowser.create();
    val page = browser.doGet(cmdOpt.url);

    cmdOpt.selector match {

      case None =>
        val body = page.response.body;
        output(body);

      case Some(selector: CmdOpt.Selector) =>
        val element = page.asInstanceOf[HtmlPage].select(selector.selector);
        val text = element.text;
        output(EncodingMania.encodeChar(text, "UTF-8"));

    }

  }

  def help(){
    println("""
wwget [option] url
""".trim);
  }

}
