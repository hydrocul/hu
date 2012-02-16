package hydrocul.hv.rosen;

trait RouteLink {

	def startPoint: String;

	def endPoint: String;

	def startTime: TrainTime;

	def endTime: TrainTime;

	def endTime2: TrainTime;

	def mkString(tail: String): String;

}

object RouteLink {

	def unapply(r: RouteLink): Option[(String, String,
		TrainTime, TrainTime, TrainTime)] = Some(
		r.startPoint, r.endPoint, r.startTime, r.endTime, r.endTime2);

}
