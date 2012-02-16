package hydrocul.hv.rosen;

trait LinkInfo {

	def startPoint: String;

	def endPoint: String;

	def getRouteLinks(time: TrainTime, time2: TrainTime): IndexedSeq[RouteLink];

}

