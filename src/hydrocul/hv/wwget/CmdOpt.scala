package hydrocul.hv.wwget;

private[wwget] case class CmdOpt (
  url: String
);

private[wwget] object CmdOpt {

  def apply(args: List[String]): CmdOpt =
    CmdOptBuilder(None).parse(args);

}

private[wwget] case class CmdOptBuilder (
  url: Option[String]
){

  def parse(args: List[String]): CmdOpt = {
    (this, args) match {
      case (_, Nil) => build;
      case (CmdOptBuilder(None), url :: tail) =>
        CmdOptBuilder(Some(url)).parse(tail);
      case _ => throw new IllegalArgumentException(args.toString);
    }
  }

  private def build: CmdOpt = {
    this match {
      case CmdOptBuilder(Some(url)) => CmdOpt(url);
      case _ => throw new IllegalArgumentException(this.toString);
    }
  }

}

