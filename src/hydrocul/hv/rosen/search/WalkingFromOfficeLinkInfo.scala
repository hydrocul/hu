package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;
import hydrocul.util.StringLib;

case class WalkingFromOfficeLinkInfo (
  startPoint: String,
  endPoint: String,
  timePairs: Seq[TrainTimePair]
) extends LinkInfo {

  override def getRoute(endPoint: String,
                        time1: TrainTime, time2: TrainTime, fromOffice: Boolean,
                        linkInfoList: String => Seq[LinkInfo]): Route = {

    // time1 以降の電車
    val a1 = timePairs.sortWith(_.start < _.start).
      dropWhile(_.start < time1);

    // time1 以降で time2 より前の電車
    val a2 = a1.takeWhile(_.start < time2);

    // time2 以降の電車
    val a3 = a1.drop(a2.length);

    val a7 = if(a3.isEmpty){
      a2;
    } else {
      a2 :+ a3.head;
    }

    val a8 = a7.sortWith(_.end < _.end).map(timePair => {

      val e1 = timePair.start;
      val e2 = timePair.end;
      val next = Route.search(this.endPoint, endPoint,
        e2, e2, true, linkInfoList);
      if(next.isDefined){
        Some(WalkingFromOfficeRoute(startPoint, e1, e2, next));
      } else {
        None;
      }

    })

    val a9 = a8.filter(_.isDefined).map(_.get)

    routeList(a9);

  }

  private def routeList(list: Seq[WalkingFromOfficeRoute]) =
    if(list.isEmpty) NoRoute(startPoint); else WalkingFromOfficeRouteList(startPoint, list);

}

case class WalkingFromOfficeRoute (
  startPoint: String,
  startTime: TrainTime,
  endTime: TrainTime,
  nextRoute: Route
) extends Route {

  override def endTime1: Option[TrainTime] = nextRoute.endTime1;

  override def endTime2: Option[TrainTime] = nextRoute.endTime2;

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    val start = startTime.toString;
    val h = "-"
    val s2 = "(" + start + ")" + h;
    val next = nextRoute.mkString(None, color);
    if(next.isEmpty){
      Nil;
    } else {
      val h = s2 + next.head;
      val s = Vector.fill(StringLib.lengthOnTerminal(s2))(" ").mkString;
      val t = next.tail.map(s + _);
      h :: t.toList;
    }
  }

  override def update(p: Route => Route): Route =
    p(WalkingFromOfficeRoute(startPoint, startTime, endTime,
      nextRoute.update(p)));

}

case class WalkingFromOfficeRouteList (
  startPoint: String,
  list: Seq[WalkingFromOfficeRoute]
) extends Route {

  lazy val endTime1: Option[TrainTime] = Some(list.map(_.endTime1.get).min);

  lazy val endTime2: Option[TrainTime] = Some(list.map(_.endTime2.get).max);

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    list.flatMap(_.mkString(prevStation, color));
  }

  override def update(p: Route => Route): Route = {
    val newList = list.map(_.update(p));
    if(newList.exists(!_.isInstanceOf[WalkingFromOfficeRoute])){
      p(SelectableRoute(startPoint, newList));
    } else {
      p(WalkingFromOfficeRouteList(startPoint, newList.asInstanceOf[Seq[WalkingFromOfficeRoute]]));
    }
  }

}

