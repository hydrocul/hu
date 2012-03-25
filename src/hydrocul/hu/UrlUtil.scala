package hydrocul.hu;

object UrlUtil {

  /**
   * ベースとなるURL(baseUrl)と相対パス(relativePath)から絶対パスのURLを生成する。
   * relativePath が絶対パスの場合はそのまま返す。
   */
  def createUrl(baseUrl: String, relativePath: String): Option[String] = {
    hydrocul.hv.http.UrlInfo(baseUrl).createUrl(relativePath).map(_.url);
/*
    def sub(baseUrl: String, relativePath: String, first: Boolean): Option[String] = {
      if(first){
        baseUrl match {
          case Pattern1(sch, host) =>
            return sub(sch + "://" + host + "/", relativePath, first);
          case _ => ; // nothing
        }
      }
      if(first){
        val p = relativePath.indexOf('#');
        if(p >= 0){
          return sub(baseUrl, relativePath.substring(0, p), first).
              map(u => u + relativePath.substring(p));
        }
      }
      val relativePathI = relativePath.toLowerCase;
      if(first && relativePathI.startsWith("mailto:")){
        None;
      } else if(first && relativePathI.startsWith("javascript:")){
        None;
      } else if(first && relativePathI.startsWith("tel:")){
        None;
      } else if(first && relativePathI.startsWith("http://")){
        Some(relativePath);
      } else if(first && relativePathI.startsWith("https://")){
        Some(relativePath);
      } else if(first && relativePath.startsWith("//")){
        val p = baseUrl.indexOf(':');
        if(p >= 0){
          Some(baseUrl.substring(0, p + 1) + relativePath);
        } else {
          throw new IllegalArgumentException("baseUrl: " + baseUrl);
        }
      } else if(first && relativePath.startsWith("/")){
        baseUrl match {
          case Pattern2(sch, host, path) => Some(sch + "://" + host + relativePath);
          case _ => throw new IllegalArgumentException("baseUrl: " + baseUrl);
        }
      } else if(relativePath.isEmpty){
        Some(baseUrl);
      } else if(relativePath.equals(".")){
        Some(createSelfDirUrl(baseUrl));
      } else if(relativePath.equals("..")){
        Some(createParentDirUrl(baseUrl));
      } else if(relativePath.startsWith("./")){
        sub(createSelfDirUrl(baseUrl), relativePath.substring(2), false);
      } else if(relativePath.startsWith("../")){
        sub(createParentDirUrl(baseUrl), relativePath.substring(3), false);
      } else {
        val p = relativePath.indexOf('/');
        if(p < 0){
          Some(createSelfDirUrl(baseUrl) + relativePath);
        } else {
          sub(createSelfDirUrl(baseUrl) + relativePath.substring(0, p + 1),
            relativePath.substring(p + 1), false);
        }
      }
    }
    sub(baseUrl.trim, relativePath.trim, true);
*/
  }

  def createSelfDirUrl(url: String): String = {
    url match {
      case Pattern1(sch, host) => url;
      case Pattern2(sch, host, path) => {
        val p1 = path.lastIndexOf('/');
        val newPath = if(p1 < 0){
          throw new AssertionError(url);
        } else {
          path.substring(0, p1 + 1)
        }
        sch + "://" + host + newPath;
      }
      case _ => throw new IllegalArgumentException("url: " + url);
    }
  }

  def createParentDirUrl(url: String): String = {
    url match {
      case Pattern1(sch, host) => url;
      case Pattern2(sch, host, path) => {
        val p1 = path.lastIndexOf('/');
        val p2 = if(p1 < 0){
          throw new AssertionError(url);
        } else {
          path.substring(0, p1).lastIndexOf('/');
        }
        val newPath = if(p2 < 0){
          "/";
        } else {
          path.substring(0, p2 + 1);
        }
        sch + "://" + host + newPath;
      }
      case _ => throw new IllegalArgumentException("url: " + url);
    }
  }

  private val Pattern1 = "(https?)://([^/]+)".r;
  private val Pattern2 = "(https?)://([^/]+)(/.*)".r;

  def test: Seq[Option[String]] = List(
    testCreateUrlSub("01", "http://aaa/bbb/ccc/", "ddd", "http://aaa/bbb/ccc/ddd"),
    testCreateUrlSub("02", "http://aaa/bbb/ccc", "ddd", "http://aaa/bbb/ddd"),
    testCreateUrlSub("03", "http://aaa/bbb/ccc/", "ddd/eee", "http://aaa/bbb/ccc/ddd/eee"),
    testCreateUrlSub("04", "http://aaa/bbb/ccc", "ddd/eee", "http://aaa/bbb/ddd/eee"),
    testCreateUrlSub("05", "http://aaa/bbb/ccc/", "../ddd/eee", "http://aaa/bbb/ddd/eee"),
    testCreateUrlSub("06", "http://aaa/bbb/ccc", "../ddd/eee", "http://aaa/ddd/eee"),
    testCreateUrlSub("07", "http://aaa/bbb/ccc/", "../../ddd", "http://aaa/ddd"),
    testCreateUrlSub("08", "http://aaa/bbb/ccc", "../../ddd", "http://aaa/ddd"),
    testCreateUrlSub("09", "http://aaa/bbb/ccc/", "./ddd", "http://aaa/bbb/ccc/ddd"),
    testCreateUrlSub("10", "http://aaa/bbb/ccc", "./ddd", "http://aaa/bbb/ddd"),
    testCreateUrlSub("21", "http://aaa/bbb/ccc", "/ddd", "http://aaa/ddd"),
    testCreateUrlSub("22", "http://aaa/bbb/ccc", "//www.google.co.jp/ddd", "http://www.google.co.jp/ddd"),
    testCreateUrlSub("23", "https://aaa/bbb/ccc", "//www.google.co.jp/ddd", "https://www.google.co.jp/ddd"),
    testCreateUrlSub("24", "http://aaa/bbb/ccc", "https://www.google.co.jp/ddd", "https://www.google.co.jp/ddd"),
    testCreateUrlSub("31", "http://aaa/", "ddd", "http://aaa/ddd"),
    testCreateUrlSub("32", "http://aaa", "ddd", "http://aaa/ddd"),
    testCreateUrlSub("41", "http://aaa/bbb", "#", "http://aaa/bbb#"),
    testCreateUrlSub("42", "http://aaa/bbb", "/ccc//", "http://aaa/ccc//")
  );

  private def testCreateUrlSub(name: String, baseUrl: String, relativePath: String,
    expected: String): Option[String] = {
    TestLib.assertEquals(name + ": " + expected, name + ": " + createUrl(baseUrl, relativePath).get);
  }

}

