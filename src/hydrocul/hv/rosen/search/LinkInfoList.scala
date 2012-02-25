package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

case class LinkInfoList(list: Seq[LinkInfo]){

  def search(startPoint: String, endPoint: String,
             time1: TrainTime, time2: TrainTime): Seq[Route] = {

    if(startPoint == endPoint){
      Vector(Route(Nil));
    } else {

      // 次に接続する RouteLink のリスト
      val a1: Seq[RouteLink] = list.filter(_.startPoint == startPoint).
        flatMap(_.getRouteLinks(time1, time2));

      val a4: Seq[Option[Seq[Route]]] = a1.map { a: RouteLink =>

        // a の終点から再度路線検索をする
        val a2: Seq[Route] = search(a.endPoint, endPoint, a.endTime1, a.endTime2);

        // a2 の先頭に a を接続して、startPoint からの路線検索の結果とする
        val a3: Seq[Route] = a2.map(a3 => Route(a :: a3.links));

        if(a3.isEmpty){
          None;
        } else {
          Some(a3);
        }

      }

      val a5: Seq[(Seq[Route], TrainTime)] = a4.filter(_.isDefined).map(_.get).
        map(a => (a, a.map(_.endTime1).min));

      // 早く到着する順番に並び替える
      val a6 = a5.sortBy(_._2).flatMap(_._1);

      a6;

    }

  }

}
