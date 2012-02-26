package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;
import hydrocul.util.StringLib;

case class TrainLinkInfo (
  startStation: String,
  endStation: String,
  startPoint: String,
  endPoint: String,
  timePairs: Seq[TrainTimePair],
  tokyoMetro: Boolean
) extends LinkInfo {

  override def getRoute(endPoint: String,
                        time1: TrainTime, time2: TrainTime,
                        linkInfoList: String => Seq[LinkInfo]): Route = {

    // time1 以降の電車
    val a1 = timePairs.sortWith(_.start < _.start).
      dropWhile(_.start < time1);

    // time1 以降で time2 より前の電車
    val a2 = a1.takeWhile(_.start < time2);

    // time2 以降の電車
    val a3 = a1.drop(a2.length);

    val a7 = if(a3.isEmpty){
      a3;
    } else {

      // time1 以降の電車で time2 以降の最初の電車まで
      val a4 = (a2 :+ a3.head);

      // a4 の中で到着が最も遅い電車の到着時刻
      val a5 = a4.map(_.end).max;

      // time2 以降の電車で、a5 と同じかより早い電車
      val a6 = a3.tail.filter(_.end <= a5);

      a4 ++ a6;

    }

    val a8 = a7.sortWith(_.end < _.end).map(timePair => {

      // time2 より前で間に合うかどうかがわからない場合に true
      val ss = timePair.start < time2;

      val e1 = timePair.start;
      val e2 = timePair.end;
      val next = Route.search(this.endPoint, endPoint,
        e2, e2, linkInfoList);
      if(next.isDefined){
        Some(TrainRoute(startStation, endStation, startPoint, endPoint,
          e1, e2, tokyoMetro, ss, next));
      } else {
        None;
      }

    })

    val a9 = a8.filter(_.isDefined).map(_.get);

    routeList(a9);

  }

  private def routeList(list: Seq[TrainRoute]) =
    if(list.isEmpty) NoRoute(startPoint);
    else TrainRouteList(startStation, endStation, startPoint, endPoint, list);

}

case class TrainRoute (
  startStation: String,
  endStation: String,
  startPoint: String,
  endPoint: String,
  startTime: TrainTime,
  endTime: TrainTime,
  tokyoMetro: Boolean,
  second: Boolean, // time2 より前で間に合うかどうかがわからない場合に true
  nextRoute: Route
) extends Route {

  override def endTime1: Option[TrainTime] = nextRoute.endTime1;

  override def endTime2: Option[TrainTime] = nextRoute.endTime2;

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    val start = {
      if(second && color) Console.RED + startTime + Console.RESET;
      else startTime.toString;
    }
    val h = {
      if(tokyoMetro) "=";
      else if(color) Console.RED + "-" + Console.RESET;
      else "-";
    }
    val StartStation = startStation;
    val s1 = prevStation match {
      case None => startStation;
      case Some(StartStation) => "";
      case _ => "-" + startStation;
    };
    val s2 = s1 + "(" + start + ")" + h + "(" + endTime + ")" + endStation;
    val next = nextRoute.mkString(Some(endStation), color);
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
    p(TrainRoute(startStation, endStation, startPoint, endPoint, startTime, endTime,
      tokyoMetro, second, nextRoute.update(p)));

}

case class TrainRouteList (
  startStation: String,
  endStation: String,
  startPoint: String,
  endPoint: String,
  list: Seq[TrainRoute]
) extends Route {

  lazy val endTime1: Option[TrainTime] = Some(list.map(_.endTime1.get).min);

  lazy val endTime2: Option[TrainTime] = Some(list.map(_.endTime2.get).max);

  override def mkString(prevStation: Option[String], color: Boolean): Seq[String] = {
    list.flatMap(_.mkString(prevStation, color));
  }

  override def update(p: Route => Route): Route = {
    val newList = list.map(_.update(p));
    if(newList.exists(!_.isInstanceOf[TrainRoute])){
      p(SelectableRoute(startPoint, newList));
    } else {
      p(TrainRouteList(startStation, endStation, startPoint, endPoint,
        newList.asInstanceOf[Seq[TrainRoute]]));
    }
  }

}

