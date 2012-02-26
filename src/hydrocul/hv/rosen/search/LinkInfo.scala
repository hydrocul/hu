package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

trait LinkInfo {

	def startPoint: String;

	def endPoint: String;

	def getRoute(endPoint: String, time1: TrainTime, time2: TrainTime, fromOffice: Boolean,
    linkInfoList: String => Seq[LinkInfo]): Route;

}

