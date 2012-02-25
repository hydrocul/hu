package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class LinkInfoList(val list: Seq[LinkInfo]){

  def search(startPoint: String, endPoint: String,
             time1: TrainTime, time2: TrainTime): Seq[List[RouteLink]] = {

    if(startPoint == endPoint){
      Vector(Nil);
    } else {

      // 次に接続する RouteLink のリスト
      val a1: Seq[RouteLink] = list.filter(_.startPoint == startPoint).
        flatMap(_.getRouteLinks(time1, time2));

      val a4: Seq[List[RouteLink]] = a1.flatMap { a: RouteLink =>

        // a の終点から再度路線検索をする
        val a2: Seq[List[RouteLink]] = search(a.endPoint, endPoint, a.endTime1, a.endTime2);

        // a2 の先頭に a を接続して、startPoint からの路線検索の結果とする
        val a3: Seq[List[RouteLink]] = a2.map(a3 => a :: a3);

        a3;

      }

      a4;

    }

  }

}
