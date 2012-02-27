package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class WalkingLinkInfo (
  startPoint: String,
  endPoint: String,
  walkingTime1: Int,
  walkingTime2: Int
) extends LinkInfo {

  override def getRoute(endPoint: String,
                        time1: TrainTime, time2: TrainTime, fromOffice: Boolean,
                        linkInfoList: String => Seq[LinkInfo]): Route = {
    val e1 = time1 + walkingTime1;
    val e2 = time2 + walkingTime2;
    val next = Route.search(this.endPoint, endPoint,
      e1, e2, false, linkInfoList);
    if(next.isDefined){
      WalkingRoute(startPoint, this.endPoint, next);
    } else {
      NoRoute(startPoint);
    }
  }

}

case class WalkingRoute (
  startPoint: String,
  endPoint: String,
  nextRoute: Route
) extends Route {

  override def endTime1: Option[TrainTime] = nextRoute.endTime1;

  override def endTime2: Option[TrainTime] = nextRoute.endTime2;

  override def endTime3: Option[TrainTime] = nextRoute.endTime3;

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    nextRoute.mkString(prevStation, color);
  }

  override def update(p: Route => Route): Route =
    p(WalkingRoute(startPoint, endPoint, nextRoute.update(p)));

}

