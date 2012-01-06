package hydrocul.hu.eval.serv;

import hydrocul.hu.IO;
import hydrocul.hu.EncodingMania;
import hydrocul.hu.File;
import hydrocul.hu.eval.Evaluator;

object EvalServer {

  def main(args: Array[String]){
    execServer.exec();
  }

  private def execServer: IO[Unit] = {
    val ev = new Evaluator;
    execServerSub(ev);
  }

  private def execServerSub(evaluator: Evaluator): IO[Unit] = {
    execOne(evaluator) then { _ =>
      execServerSub(evaluator);
    }
  }

  private def execOne(evaluator: Evaluator): IO[Unit] = {
    getScriptFile then {
      case None =>
        IO.sleep(300);
      case Some(fpath) =>
        execFile(evaluator, fpath) then { result =>
          outputResult(fpath, result);
        }
    }
  }

  private def getScriptFile: IO[Option[String]] = {
    File(tmpDir).list map { files =>
      files.keySet.find {
        case FileNamePattern(fname) => true;
        case _ => false;
      } map { fname => tmpDir + "/" + fname; }
    }
  }

  private def execFile(evaluator: Evaluator, fpath: String): IO[String] = {
    File(fpath).read then {
      case None =>
        IO()("");
      case Some(bin) =>
        val source = EncodingMania.decodeChar(bin, "UTF-8");
        evaluator.eval(source) map {
          case (None, msg) =>
            msg;
          case (Some(ret), _) =>
            ret.toString;
        }
    }
  }

  private def outputResult(fpath: String, result: String): IO[Unit] = {
    val bin = EncodingMania.encodeChar(result, "UTF-8");
    {
      File(fpath + ".out.1").write(Some(bin));
    } then { _ =>
      File(fpath + ".out.0").write(Some(Array()));
    } then { _ =>
      File(fpath).write(None);
    }
  }

  private val tmpDir = "/tmp";
  private val FileNamePattern = "(huevalserver\\..+\\.scala)".r;

}
