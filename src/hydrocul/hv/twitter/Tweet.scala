package hydrocul.hv.twitter;

import hydrocul.hv.DateStringUtil;

trait Tweet {

  def id: Long;

  def timeStamp: Long;

  def text: String;

  def retweeted: Option[Long];

  override def toString(): String = {
    val s = (new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ",
      java.util.Locale.US)).format(new java.util.Date(timeStamp * 1000));
    "%d %s: %s".format(id, s, text);
  }

}

private[twitter] object Tweet {

  /**
   * REST APIのJSONデータからTweetインスタンスを生成する。
   */
  def fromJson(json: Any): Tweet = {

    val json2 = json.asInstanceOf[Map[String, Any]];

    val id = json2("id_str").asInstanceOf[String].toLong;

    // 投稿日時
    val timeStamp = DateStringUtil.parse(json2("created_at").asInstanceOf[String]) / 1000;

    // 生のテキスト
    val text = json2("text").asInstanceOf[String]; // + json2.toString; // DEBUG

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

    new TweetImpl(id, timeStamp, text2, retweet);

  }

  private[twitter] class TweetImpl(_id: Long, _timeStamp: Long, _text: String,
    _retweeted: Option[Long]) extends Tweet {

    def id: Long = _id;

    def timeStamp: Long = _timeStamp;

    def text: String = _text;

    def retweeted: Option[Long] = _retweeted;

  }

}

