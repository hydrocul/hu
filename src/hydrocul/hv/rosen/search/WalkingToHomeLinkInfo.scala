package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class WalkingToHomeLinkInfo (
  startPoint: String,
  endPoint: String,
  walkingTime: Int
) extends LinkInfo {

  override def getRoute(endPoint: String,
                        time1: TrainTime, time2: TrainTime,
                        linkInfoList: String => Seq[LinkInfo]): Route = {
    if(endPoint != this.endPoint){
      Route.NoRoute;
    } else {
      WalkingToHomeRoute(time1 + walkingTime);
    }
  }

  private case class WalkingToHomeRoute (
    endTime: TrainTime
  ) extends Route {

    override def endTime1: Option[TrainTime] = Some(endTime);

    override def endTime2: Option[TrainTime] = Some(endTime);

    override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
      val end = {
        if(color) Console.BLUE + endTime + Console.RESET;
        else endTime.toString;
      }
      "-(" + end + ")" :: Nil;
    }

  }

}
