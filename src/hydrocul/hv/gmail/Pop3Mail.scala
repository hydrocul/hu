package hydrocul.hv.gmail;

case class Pop3Mail (
  messageId: String,
  subject: String,
  date: String,
  from: String,
  to: Seq[String],
  body: String
);
