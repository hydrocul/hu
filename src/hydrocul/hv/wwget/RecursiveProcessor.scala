package hydrocul.hv.wwget;

import hydrocul.hv.http.UrlInfo;

private[wwget] class RecursiveProcessor(opt: CmdOpt.Recursive){

  // URLを見て、クローリングの対象かどうかを判断し、
  // 対象の場合は、BASIC認証の情報を含めたURLと出力先ファイル名を返す。
  // 対象外の場合はNoneを返す。
  def eatUrl(url: String): Option[(String, String)] = {

    def catching[A](v: =>A): Option[A] = try {
      Some(v);
    } catch { case _ =>
      None;
    }

    val urlInfo = {
      val u = UrlInfo(url);
      if(!u.path.isEmpty){
        u;
      } else {
        UrlInfo(u.scheme, u.usernameAndPassword, u.host, u.port,
          "/", u.query, None); // ドメインまたはポート番号で終わる場合は "/" を最後に付ける
      }
    }

    if(
      (!opt.scheme.isDefined || urlInfo.scheme == opt.scheme.get) &&
      urlInfo.host == opt.host &&
      // TODO ポートの比較が作りかけ
      // !opt.port.isDefined &&
      //     (!urlInfo.port.isDefined ||
      //       urlInfo.port == UrlInfo.defaultPort(urlInfo.scheme)) ||
      //   opt.port.isDefined &&
      //     (!urlInfo.port.isDefined && opt.port == UrlInfo.defaultPort(urlInfo.scheme) ||
      //       urlInfo.port == UrlInfo.defaultPort(urlInfo.scheme)) || ...
      (urlInfo.path == opt.basePath ||
        urlInfo.path.startsWith(opt.basePath + "/"))
    ){ // クローリング対象の場合

      val fpath = { // 出力先ファイルパス
        val rel = {
          val b = opt.basePath;
          val p = if(b.isEmpty) 0 else b.lastIndexOf("/");
            // b は必ず空文字列または "/" から始まる文字列
          urlInfo.path.substring(p + 1);
        }
        opt.outputPath + "/" + segmentsToFilePath(rel);
      }

      val newUrlInfo = UrlInfo(urlInfo.scheme, opt.usernameAndPassword,
        urlInfo.host, urlInfo.port, urlInfo.path, urlInfo.query, None);
      Some((newUrlInfo.urlWithAuth, fpath));

    } else { // クローリングの対象外の場合
      None;
    }

  }

  /**
   * "aaa/bbb/" を "aaa/bbb/index.html" に変換する
   */
  private def segmentsToFilePath(segments: String): String = {
    if(segments.endsWith("/")){
      segments + "index.html";
    } else {
      segments;
    }
  }

  /*
  private def segmentToFileName(segment: String): String = {
    segment;
  }
  */

}
