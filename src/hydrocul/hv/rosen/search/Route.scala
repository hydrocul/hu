package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

trait Route {

  def isDefined: Boolean = endTime1.isDefined;

  def endTime1: Option[TrainTime];

  def endTime2: Option[TrainTime];

  def mkString(color: Boolean): Seq[String] = mkString(None, color);

  def mkString(prevStation: Option[String], color: Boolean): Seq[String];

}

object Route {

  private def terminator(time1: TrainTime, time2: TrainTime): Route =
    TerminatorRoute(time1, time2);

  private def selectable(routeList: Seq[Route]): Route =
    if(routeList.isEmpty) NoRoute; else SelectableRoute(routeList);

  def search(startPoint: String, endPoint: String,
             time1: TrainTime, time2: TrainTime,
             linkInfoList: String => Seq[LinkInfo]): Route = {

    if(startPoint == endPoint){
      terminator(time1, time2);
    } else {

      // 次に接続する Route のリスト
      val a1: Seq[Route] = linkInfoList(startPoint).
        map(_.getRoute(endPoint, time1, time2, linkInfoList));

      // 終点に到達しない Route を削除
      val a2 = a1.filter(_.isDefined);

      // 早く到着する順番に並び替える
      val a3 = a2.sortBy(_.endTime1.get);

      selectable(a3);

    }

  }

}

object NoRoute extends Route {

  override def endTime1: Option[TrainTime] = None;

  override def endTime2: Option[TrainTime] = None;

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] =
    "" :: Nil;

}

case class TerminatorRoute(time1: TrainTime, time2: TrainTime) extends Route {

  override def endTime1: Option[TrainTime] = Some(time1);

  override def endTime2: Option[TrainTime] = Some(time2);

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] =
    "" :: Nil;

}

case class SelectableRoute(routeList: Seq[Route]) extends Route {

  lazy val endTime1: Option[TrainTime] = Some(routeList.map(_.endTime1.get).min);

  lazy val endTime2: Option[TrainTime] = Some(routeList.map(_.endTime2.get).max);

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    routeList.flatMap(_.mkString(prevStation, color));
  }

}

