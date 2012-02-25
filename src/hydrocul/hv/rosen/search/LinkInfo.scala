package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

trait LinkInfo {

	def startPoint: String;

	def endPoint: String;

	def getRouteLinks(time1: TrainTime, time2: TrainTime): Seq[RouteLink];

}

