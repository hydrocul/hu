package hydrocul.hu.eval;

/*
import java.{ io => jio }

import hydrocul.hu.File;
import hydrocul.hu.IO;
import hydrocul.util.CipherUtil;
import hydrocul.util.EncodingMania;
import hydrocul.util.StreamUtil;
import hydrocul.hu.task.TimeOut;

class ShellEvaluator extends Evaluator {

/*

  def loadObject(filePath: String, valName: String)(implicit fileIO: FileIO): Option[Any] =
    loadObject(filePath, "", valName);

  private[this] def loadObject(filePath: String, childPath: String, valName: String)(
      implicit fileIO: FileIO): Option[Any] = {

    fileIO.file(filePath).read match {
      case Some(bin) => parseObjectText(valName, bin);
      case None => None;
    }

  }

  private[this] def parseObjectText(valName: String, bin: Array[Byte]): Option[Any] = {
    val source = EncodingMania.decodeChar(bin, "UTF-8");
    val (code, hash, typeStr, serialized) = source match {
      case ShellEvaluator.SepPattern(code, hash, typeStr, serialized) =>
        (code, hash, typeStr, serialized);
      case _ =>
        (source, "", "", "");
    }
    if(!hash.isEmpty && {
      val hashExpected = CipherUtil.encodeHex(CipherUtil.binaryToHash(
        EncodingMania.encodeChar(code, "UTF-8")));
      hash == hashExpected;
    }){ // シリアライズ済みの場合
      val bin = EncodingMania.decodeBase64(serialized.replaceAll("(?m)^// ", ""));
      val value = StreamUtil.bin2obj(bin);
      bind(valName, typeStr, value);
      Some(value);
    } else { // シリアライズがまだの場合
      val source = "val " + valName + " = (" + code + ");";
      val result = eval(source);
      result._1;
      // TODO シリアライズして保存する処理が必要
    }
  }

  def evalCommand(currDir: String, command: String, arg: String, output: jio.OutputStream){
    val writer = new jio.PrintWriter(new jio.OutputStreamWriter(output, "UTF-8"));
/* // TODO
    val code2 = code.trim;
    writer.println(code2);
    val t = eval(code2);
    outputResult(t._1, t._2, writer);
*/
    writer.println(currDir);
    writer.println(command);
    writer.println(arg);
    writer.flush();
  }

  private[this] def outputResult(resultVal: Option[Any], resultMsg: String,
      writer: jio.PrintWriter){
    writer.println(resultMsg);
  }

/*
  private[this] def execLoadCommand(currDir: String, arg: String){
    val FileNamePattern = "([a-zA-Z_][a-zA-Z_0-9]*).scala".r;
    val valName = arg match {
      case FileNamePattern(valName) => valName;
    }
    val filePath = currDir + "/" + 
    loadObject(
  }
*/

*/

  private def evalCommand(currDir: String, command: String): IO[(Option[Any], String)] = {
    eval(command) >>= { t =>
      t._1 match {
        case Some(r) =>
          r match {
            case io: IO[_] =>
              io >>= { result =>
                IO()((t._1, "execute: " + command + "\n" + t._2 +
                  "action: " + io + "\nresult: " + result + "\n"));
              }
            case _ =>
              IO()((t._1, "execute: " + command + "\n" + t._2));
          }
        case _ =>
          IO()((t._1, "execute: " + command + "\n" + t._2));
      }
    }
  }

}

object ShellEvaluator {

  def main(): IO[Unit] = {

    val iodir = System.getProperty("user.home") + jio.File.separator + "/.hu/eval";
    val baseFile = File(iodir);

    execMain(baseFile);

  }

  def execMain(baseFile: File): IO[Unit] = {
    waitAndGetString(baseFile) >>= { t =>
      val fname = t._1;
      val content = t._2;
      val (currDir, command) = parseCommand(content);
      evaluator.evalCommand(currDir, command) >>= { t =>
        val resultVal = t._1;
        val resultMsg = t._2;
        output(fname, baseFile, resultVal, resultMsg);
      }
    } >>= { u => execMain(baseFile); }
  }

  private lazy val evaluator = {
    val ev = new ShellEvaluator;
    ev.bind("evaluator", "hydrocul.hu.eval.Evaluator", ev);
    ev.eval("import hydrocul.hu._");
    ev;
  }

  /**
   * 入力ディレクトリをポーリングして、入力ファイルが現れたら、
   * ファイルパスと中のテキストを取得する。
   */
  private def waitAndGetString(baseFile: File): IO[(String, String)] = {
    baseFile.list >>= { list: Map[String, File] =>
      list.find { kv: (String, File) =>
        !kv._1.startsWith(".");
      } match {
        case None =>
          IO.sleep(TimeOut(200)) >>= waitAndGetString(baseFile);
        case Some((fname, file)) =>
          file.read >>= { a: Option[Array[Byte]] => a match {
            case Some(bin) =>
              file.write(None) >>=
              IO()((fname, EncodingMania.decodeChar(bin, "UTF-8")));
            case None =>
              IO.sleep(TimeOut(200)) >>= waitAndGetString(baseFile);
          } }
      }
    }
  }

  private def output(outputPath: String, baseFile: File,
      resultVal: Option[Any], resultMsg: String): IO[Unit] = {

    def output(n: Int, result: String): IO[Unit] = {
      val bin = EncodingMania.encodeChar(result, "UTF-8");
      val fname = "." + outputPath + ".out." + n;
      baseFile.getByPath(fname).get.write(Some(bin));
    }

    def output0(): IO[Unit] = {
      val fname = "." + outputPath + ".out." + "0";
      baseFile.getByPath(fname).get.write(Some(Array[Byte](0)));
    }

    output(1, resultMsg) >>=
    output0();

  }

  /**
   * 入力ファイルに書かれた文字列から、
   * カレントディレクトリとコマンドを取得する
   */
  private def parseCommand(code: String): (String, String) = {
    code match {
      case CommandPattern2(s1, s2) => (s1, s2);
      case _ => ("", code);
    }
  }

  private def splitCommand(source: String): (String, String) = {
    val r = source.split("\\s", 2);
    if(r.length == 1){
      (r(0), "");
    } else {
      (r(0), r(1));
    }
  }

  private val CommandPattern2 = "(?s)([^:]+):(.+)".r;

}
*/
