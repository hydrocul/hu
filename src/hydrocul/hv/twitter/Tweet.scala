package hydrocul.hv.twitter;

trait Tweet {

  def text: String;

  def toString(): String;

}

private[twitter] object Tweet {

  def apply(json: Any): Tweet = {
    val json2 = json.asInstanceOf[Map[String, Any]];
    val text = json2("text").asInstanceOf[String];
    new TweetImpl(text);
  }

  private[twitter] class TweetImpl(_text: String) extends Tweet {

    def text: String = _text;

    override def toString(): String = _text;

  }

}

