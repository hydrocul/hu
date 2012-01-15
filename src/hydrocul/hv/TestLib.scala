package hydrocul.hv;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.regex.Pattern;

import hydrocul.util.StreamUtil;

object TestLib {

  def assertTrue(actual: Boolean): Option[String] = {
    if(actual){
      None;
    } else {
      val msg = "[ERROR] expected: %s\n  but     actual: %s\n%s".
        format("true", "false", getStackTrace(3));
      Some(msg);
    }
  }

  def assertEquals(expected: Any, actual: Any): Option[String] = {
    if(expected == actual){
      None;
    } else {
      val msg = "[ERROR] expected: %s\n  but     actual: %s\n%s".
        format(expected, actual, getStackTrace(3));
      Some(msg);
    }
  }

  def assertMatches(regex: String, actual: String): Option[String] = {
    if(Pattern.compile(regex).matcher(actual).matches()){
      None;
    } else {
      val msg = "[ERROR] expected: %s\n  but     actual: %s\n%s".
        format(regex, actual, getStackTrace(3));
      Some(msg);
    }
  }

  private def getStackTrace(level: Int): String = {
    StreamUtil.exception2stackTrace(new AssertionError).split("\n")(level).trim;
  }

}
