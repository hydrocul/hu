package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

trait Route {

  def isDefined: Boolean = endTime1.isDefined;

  def startPoint: String;

  def endTime1: Option[TrainTime];

  def endTime2: Option[TrainTime];

  def endTime3: Option[TrainTime];

  def mkString(color: Boolean): Seq[String] = mkString(None, color);

  def mkString(prevStation: Option[String], color: Boolean): Seq[String];

  def update(p: Route => Route): Route;

}

object Route {

  private def terminator(startPoint: String, time1: TrainTime, time2: TrainTime): Route =
    TerminatorRoute(startPoint, time1, time2);

  def selectable(startPoint: String, routeList: Seq[Route]): Route =
    if(routeList.isEmpty) NoRoute(startPoint);
    else SelectableRouteList(startPoint, routeList);

  def search(startPoint: String, endPoint: String,
             time1: TrainTime, time2: TrainTime,
             linkInfoList: String => Seq[LinkInfo]): Route = {
    search(startPoint, endPoint, time1, time2, false, linkInfoList);
  }

  private[search] def search(startPoint: String, endPoint: String,
                             time1: TrainTime, time2: TrainTime, fromOffice: Boolean,
                             linkInfoList: String => Seq[LinkInfo]): Route = {

    if(startPoint == endPoint){
      terminator(startPoint, time1, time2);
    } else {

      // 次に接続する Route のリスト
      val a1: Seq[Route] = linkInfoList(startPoint).
        map(_.getRoute(endPoint, time1, time2, fromOffice, linkInfoList));

      // 終点に到達しない Route を削除
      val a2 = a1.filter(_.isDefined);

      // 早く到着する順番に並び替える
      val a3 = a2.sortBy(_.endTime1.get);

      selectable(startPoint, a3);

    }

  }

}

case class NoRoute(startPoint: String) extends Route {

  override def endTime1: Option[TrainTime] = None;

  override def endTime2: Option[TrainTime] = None;

  override def endTime3: Option[TrainTime] = None;

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] =
    "" :: Nil;

  override def update(p: Route => Route): Route = p(this);

}

case class TerminatorRoute(startPoint: String, time1: TrainTime, time2: TrainTime) extends Route {

  override def endTime1: Option[TrainTime] = Some(time1);

  override def endTime2: Option[TrainTime] = Some(time1);

  override def endTime3: Option[TrainTime] = Some(time2);

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] =
    "" :: Nil;

  override def update(p: Route => Route): Route = p(this);

}

trait RouteList extends Route {

  def routeList: Seq[Route];

  override def startPoint: String;

  override lazy val endTime1: Option[TrainTime] = {
    if(routeList.isEmpty){
      None;
    } else {
      Some(routeList.map(_.endTime1.get).min);
    }
  }

  override lazy val endTime2: Option[TrainTime] = {
    if(routeList.isEmpty){
      None;
    } else {
      Some(routeList.map(_.endTime2.get).min);
    }
  }

  override lazy val endTime3: Option[TrainTime] = {
    if(routeList.isEmpty){
      None;
    } else {
      Some(routeList.map(_.endTime3.get).max);
    }
  }

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    routeList.flatMap(_.mkString(prevStation, color));
  }

  override def update(p: Route => Route): Route = p(this);

}

case class SelectableRouteList(startPoint: String, routeList: Seq[Route]) extends RouteList {

  override def update(p: Route => Route): Route =
    p(SelectableRouteList(startPoint, routeList.map(_.update(p))));

}

