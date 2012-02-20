package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

trait LinkInfo {

	def startPoint: String;

	def endPoint: String;

	def getRouteLinks(time: TrainTime, time2: TrainTime): IndexedSeq[RouteLink];

}

