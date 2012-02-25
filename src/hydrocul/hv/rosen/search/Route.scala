package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

case class Route(links: List[RouteLink]){

  def endTime1 = links.last.endTime1;

  def endTime2 = links.last.endTime2;

  def mkString(color: Boolean): String = {
    links match {
      case Nil => "";
      case head :: tail => head.mkString(Route(tail).mkString(color), color);
    }
  }

}


