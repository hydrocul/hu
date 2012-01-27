package hydrocul.hv.twitter;

trait Tweet {

  def id: Long;

  def text: String;

  def toString(): String;

}

private[twitter] object Tweet {

  /**
   * REST APIのJSONデータからTweetインスタンスを生成する。
   */
  def fromJson(json: Any): Tweet = {

    val json2 = json.asInstanceOf[Map[String, Any]];

    val id = json2("id_str").asInstanceOf[String].toLong;

    // 生のテキスト
    val text = json2("text").asInstanceOf[String];

    // t.co の短縮URLを展開したテキスト
    val text2 = {
    val urls = json2("entities").asInstanceOf[Map[String, Seq[Map[String, Any]]]]("urls");
      urls.foldLeft(text)((text, u) =>
        text.replace(u("url").asInstanceOf[String],
        u("expanded_url").asInstanceOf[String]));
    }

    // 公式リツイートの場合はツイート元のIDのSome
    val retweet = json2.get("retweeted_status").map(j =>
      j.asInstanceOf[Map[String, Any]]("id_str").asInstanceOf[String].toLong);

    new TweetImpl(id, text2, retweet);

  }

  private[twitter] class TweetImpl(_id: Long, _text: String, retweet: Option[Long]) extends Tweet {

    def id: Long = _id;

    def text: String = _text;

    override def toString(): String = "%d%s: %s".format(id,
      retweet.map(r => " RT(%d)".format(r)).getOrElse(""), text);

  }

}

