package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class WalkingLinkInfo (
  startPoint: String,
  endPoint: String,
  walkingTime1: Int,
  walkingTime2: Int
) extends LinkInfo {

  override def getRouteLinks(time1: TrainTime,
                             time2: TrainTime): IndexedSeq[RouteLink] = {
    Vector(WalkingRouteLink(time1, time1 + walkingTime1, time2 + walkingTime2))
  }

  private case class WalkingRouteLink (
    startTime: TrainTime,
    endTime1: TrainTime,
    endTime2: TrainTime
  ) extends RouteLink {

    override def startPoint: String = WalkingLinkInfo.this.startPoint;

    override def endPoint: String = WalkingLinkInfo.this.endPoint;

    override def mkString(tail: String, color: Boolean): String = tail;

  }

}
