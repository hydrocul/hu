package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class TrainLinkInfo (
  startStation: String,
  endStation: String,
  startPoint: String,
  endPoint: String,
  timePairs: IndexedSeq[TrainTimePair],
  walkingFromOffice: Boolean,
    // 会社から最寄り駅までの徒歩ルートを表す場合に true。
    // この場合、実際には電車ではなく徒歩。
  tokyoMetro: Boolean
) extends LinkInfo {

  override def getRouteLinks(time1: TrainTime,
                             time2: TrainTime): Seq[RouteLink] = {
    val a1 = timePairs.sortWith(_.start < _.start).
      dropWhile(_.start < time1); // time1 以降の電車
    val a2 = a1.takeWhile(_.start < time2); // time1 以降で time2 より前の電車
    val a3 = a1.drop(a2.length); // time2 以降の電車
    val a7 = if(a3.isEmpty){
      a3;
    } else {
      val a4 = (a2 :+ a3.head); // time1 以降の電車で time2 以降の最初の電車まで
      val a5 = a4.max(new Ordering[TrainTimePair]{
        def compare(x: TrainTimePair, y: TrainTimePair): Int = x.end - y.end;
      }); // a4 の中で到着が最も遅い電車の到着時刻
      val a6 = a3.tail.filter(_.end <= a5.end); // tim2 以降の電車で、a5 と同じかより早い電車
      a4 ++ a6;
    }
    a7.sortWith(_.end < _.end).map(timePair => {
      val ss = timePair.start < time2; // time2 より前で間に合うかどうかがわからない場合に true
      TrainRouteLink(timePair.start, timePair.end, ss)
    })
  }

  private case class TrainRouteLink (
    startTime: TrainTime,
    endTime1: TrainTime,
    second: Boolean // time2 より前で間に合うかどうかがわからない場合に true
  ) extends RouteLink {

    override def startPoint: String = TrainLinkInfo.this.startPoint;

    override def endPoint: String = TrainLinkInfo.this.endPoint;

    override def endTime2: TrainTime = endTime1;

    override def mkString(tail: String, color: Boolean): String = {
      val t = {
        if(second && color) Console.RED + startTime + Console.RESET;
        else startTime.toString;
      }
      val h = {
        if(walkingFromOffice) "=";
        else if(tokyoMetro) "=";
        else if(color) Console.RED + "-" + Console.RESET;
        else "-";
      }
      if(walkingFromOffice){
        startStation + "(" + t + ")" + h + tail;
      } else if(tail.startsWith(endStation)){
        startStation + "(" + t + ")" + h + "(" + endTime1 + ")" + tail;
      } else {
        startStation + "(" + t + ")" + h + "(" + endTime1 + ")" + endStation + "-" + tail;
      }
    }

  }

}

