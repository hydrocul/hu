package hydrocul.hu.eval;

import java.{ io => jio }

import scala.tools.nsc;

import hydrocul.hu.CipherUtil;
import hydrocul.hu.EncodingMania;
import hydrocul.hu.File;
import hydrocul.hu.IO;
import hydrocul.hu.task.Synchronizer;
import hydrocul.util.StreamUtil;
import hydrocul.util.SwitchWriter;

class Evaluator {

  private[this] val sync = new Synchronizer;

  def eval(code: String): IO[(Option[Any], String)] = {
    interpreter.using[IO[(Option[Any], String, String)]]{ (main, bufGetter) =>
      IO[(IO[Option[Any]], String, String)](sync){
        try {
          val result = main.interpret(code);
          val name = main.mostRecentVar;
          val value: IO[Option[Any]] = valueOf[Any](name);
          if(result == nsc.interpreter.Results.Error){
            (IO()(None), "", bufGetter());
          } else {
            (value, name, bufGetter());
          }
        } catch {
          case e =>
            (IO()(None), "", e.toString);
        }
      } >>= { t =>
        val value: IO[Option[Any]] = t._1;
        val name = t._2;
        val msg = t._3;
        value map { opt =>
          (opt, name, msg);
        }
      }
    } map { t =>
      val result: Option[Any] = t._1;
      val name = t._2;
      val msg = t._3;
      (result, msg);
    }
  }

  def valueOf[A: Manifest](name: String) = IO[Option[A]](sync)(
    interpreter.main.valueOfTerm(name).map(_.asInstanceOf[A]));

  def typeOf(name: String) = IO[Option[String]](sync)(
    interpreter.main.typeOfTerm(name).map(_.toString));

  def bind(name: String, boundType: String, value: Any) = IO[Unit](sync){
    interpreter.using[Unit]{ (main, bufGetter) =>
      main.bind(name, boundType, value);
    }
  }

  private[this] lazy val interpreter = new {

    private[this] val sw = new SwitchWriter();

    val main: nsc.interpreter.IMain = {
      System.setProperty("scala.usejavacp", "true");
      val settings = new nsc.Settings;
      settings.usejavacp.value = true;
      new nsc.interpreter.IMain(settings, new jio.PrintWriter(sw));
    }

    def using[A](p: (nsc.interpreter.IMain, ()=>String) => A): A = {
      val buf = new jio.StringWriter();
      sw.setWriter(buf);
      p(main, ()=>{
        sw.flush();
        buf.toString;
      });
    }

  }

  def loadObject(valName: String, file: File): IO[Option[Any]] = {
    file.read >>= { opt =>
      opt match {
        case None =>
          IO()(None);
        case Some(bin) =>
          parseObjectText(valName, EncodingMania.decodeChar(bin, "UTF-8"), file) map { v =>
            Some(v);
          }
      }
    }
  }

  private[this] def parseObjectText(valName: String, source: String,
      file: File): IO[Any] = {
    val (codeTmp, hash, typeStr, serialized) = source match {
      case Evaluator.SepPattern(code, hash, typeStr, serialized) =>
        (code, hash, typeStr, serialized);
      case _ =>
        (source, "", "", "");
    }
    val code = if(codeTmp.endsWith("\n")){
      codeTmp;
    } else {
      codeTmp + "\n";
    }
    if(!hash.isEmpty && {

      // ソースコードから計算するハッシュ値
      val hashExpected = CipherUtil.encodeHex(CipherUtil.binaryToHash(
        EncodingMania.encodeChar(code, "UTF-8")));

      // 保存されているハッシュ値とソースコードから計算したハッシュ値が一致するかどうか
      hash == hashExpected;

    }){
      // シリアライズ済みの場合

      val bin = EncodingMania.decodeBase64(serialized.replaceAll("(?m)^// ", ""));
      val value = StreamUtil.bin2obj(bin);
      bind(valName, typeStr, value) >>=
      { _ =>
        IO()(value);
      }

    } else {
      // シリアライズがまだの場合

      val source = "val " + valName + " = { " + code + " }";
      eval(source) map { t =>
        t._1 match {
          case Some(r) => r;
          case _ => throw new Exception(t._2);
        }
      } >>=
      { result =>
        val binOpt = try {
          Some(StreamUtil.obj2bin(result.asInstanceOf[AnyRef]));
            // TODO asInstanceOf[AnyRef] はとりあえずの対応
        } catch { case e: java.io.NotSerializableException =>
          None;
        }
        binOpt match {
          case Some(bin) =>
            typeOf(valName) >>= {
              case Some(typeStr) =>
                val serialized = EncodingMania.encodeBase64(bin).replaceAll("(?m)^", "// ");
                val hash = CipherUtil.encodeHex(CipherUtil.binaryToHash(
                  EncodingMania.encodeChar(code, "UTF-8")));
                val serializedSource = code + "// --serialized--" + hash +
                  "--" + typeStr + "--\n" + serialized;
                file.write(Some(EncodingMania.encodeChar(serializedSource, "UTF-8"))) map { _ =>
                  result;
                }
              case None =>
                throw new Exception();
            }
          case None =>
            IO()(result);
        }
      }

    }
  }

}

private[eval] object Evaluator {

  private val SepPattern = "(?ms)(.*)^// --serialized--([a-fA-F0-9]+)--([^-]+)--$(.*)".r;

}

