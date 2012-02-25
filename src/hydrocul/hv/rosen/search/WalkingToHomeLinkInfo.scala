package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class WalkingToHomeLinkInfo (
  startPoint: String,
  endPoint: String,
  walkingTime: Int
) extends LinkInfo {

  override def getRouteLinks(time: TrainTime,
                             time2: TrainTime): Seq[RouteLink] = {
    Vector(WalkingToHomeRouteLink(time, time + walkingTime))
  }

  private case class WalkingToHomeRouteLink (
    startTime: TrainTime,
    endTime1: TrainTime
  ) extends RouteLink {

    override def startPoint: String = WalkingToHomeLinkInfo.this.startPoint;

    override def endPoint: String = WalkingToHomeLinkInfo.this.endPoint;

    override def endTime2: TrainTime = endTime1;

    override def mkString(tail: String, color: Boolean): String = {
      if(!tail.isEmpty)
        throw new IllegalArgumentException(tail);
      val t = {
        if(color) Console.BLUE + endTime1 + Console.RESET;
        else endTime1.toString;
      }
      "-(" + t + ")" + tail;
    }

  }

}
