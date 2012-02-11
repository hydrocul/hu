package hydrocul.hv;

object DateStringUtil {

  def parse(dateTimeStr: String): Long = {
    List(
      "EEE, dd MMM yyyy HH:mm:ss zzz", // RFC1123
      "EEE MMM dd hh:mm:ss zzz yyyy"
    ).foldLeft[Option[Long]](None){ (o, f) => o.orElse {
      val format = new java.text.SimpleDateFormat(f, java.util.Locale.US);
      try {
        val date = format.parse(dateTimeStr);
        Some(date.getTime);
      } catch { case _: java.text.ParseException =>
        None;
      }
    } } getOrElse {
      throw new IllegalArgumentException(dateTimeStr);
    }
  }

  private[hv] def test(): Seq[Option[String]] = {
    import TestLib._;
    List(
      assertEquals(0L,
        parse("Thu Jan 01 00:00:00 +0000 1970")),
      assertEquals(86400L * 1000,
        parse("Thu Jan 02 00:00:00 +0000 1970")),
      assertEquals(1328612715000L,
        parse("Tue Feb 07 11:05:15 +0000 2012")),
      assertEquals(1328612715000L,
        parse("Tue Feb 07 20:05:15 GMT+09:00 2012"))
    );
  }

}
