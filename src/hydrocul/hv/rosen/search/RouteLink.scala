package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen.TrainTime;

trait RouteLink {

	def startPoint: String;

	def endPoint: String;

	def startTime: TrainTime;

	def endTime1: TrainTime;

	def endTime2: TrainTime;

	def mkString(tail: String, color: Boolean): String;

}
