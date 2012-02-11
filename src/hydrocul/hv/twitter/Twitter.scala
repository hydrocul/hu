package hydrocul.hv.twitter;

import hydrocul.hv.EncodingMania;
import hydrocul.hv.http;

object Twitter {

  def userTimeline(screenName: String): Seq[Tweet] =
    userTimeline(screenName, 20, None);

  /**
   * user_timeline を取得する。maxIdを指定した場合は、
   * maxId以下(同じかより古い)のIDのtweetを取得する。
   */
  def userTimeline(screenName: String, count: Int, maxId: Option[Long]): Seq[Tweet] = {
    val url = ("http://api.twitter.com/1/statuses/user_timeline.json?" +
      "include_entities=true&include_rts=true&screen_name=%s&count=%d").format(
      EncodingMania.encodeUrl(screenName, "UTF-8"), count) +
      maxId.map("&max_id=" + _.toString).getOrElse("");
    val page = http.WebBrowser.doGet(url);
    val json: Seq[Any] = page match {
      case page: http.JsonPage =>
        page.value match {
          case json: Seq[_] => json;
          case json =>
            throw new Exception(json.toString);
        }
      case _ => throw new Exception(page.toString);
    }
    json.map(Tweet.fromJson(_));
  }

}
