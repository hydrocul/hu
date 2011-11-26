package hydrocul.util;

import java.{ io => jio }

object StdoutSwitcher {

  private lazy val switcher = new ThreadLocalSwitchOutputStream(System.out);
  private var initialized: Boolean = false;

  /**
   * usingThreadLocalStdout を使えるように準備する。
   * このメソッドはJVM全体に影響を及ぼす。
   */
  def initThreadLocalStdout(){
    if(!initialized){
      initialized = true;
      val p = new jio.PrintStream(switcher);
      System.setOut(p);
      System.setErr(p);
    }
  }

  def usingThreadLocalStdout[A](out: jio.OutputStream)(p: => A): A = {
    val old = switcher.getOutputStream();
    try {
      switcher.setOutputStream(out);
      p;
    } finally {
      switcher.setOutputStream(old);
    }
  }

}
