package hydrocul.hv.rosen.search;

import hydrocul.hv.rosen._;

/*
case class RouteSearchResult (
	startPoint: String,
	endPoint: String,
	time1: TrainTime,
	time2: TrainTime,
	next: IndexedSeq[IndexedSeq[RouteSearchResult.NextResult]]
){

	def isValid: Boolean = (startPoint==endPoint || !next.isEmpty);

	def finalEndTime: TrainTime = {
		if(startPoint==endPoint){
			time1
		} else {
			next(0)(0).result.finalEndTime
		}
	}

	def mkString: IndexedSeq[String] = {
		if(startPoint==endPoint){
			Vector(endPoint)
		} else {
			next.flatMap(_.flatMap(r => {
				r.result.mkString.map(r.link.mkString(_))
			}))
		}
	}

}

object RouteSearchResult {

	def search (
		linkInfoList: IndexedSeq[LinkInfo],
		startPoint: String,
		endPoint: String,
		time1: TrainTime,
		time2: TrainTime,
		startOffice: Boolean
	): RouteSearchResult = {
		val next: IndexedSeq[IndexedSeq[NextResult]] =
			if(startPoint==endPoint){
				Vector.empty[IndexedSeq[NextResult]]
			} else {
				val a: IndexedSeq[IndexedSeq[NextResult]] =
					linkInfoList.filter(_.startPoint==startPoint).
					map(_.getRouteLinks(time1, time2)).
					filter(!_.isEmpty).map(_.map(r =>
						NextResult(r, search(
							linkInfoList, r.endPoint, endPoint,
							r.endTime, r.endTime2, false)))).
					map(_.filter(r => r.result.isValid)).
					filter(!_.isEmpty);
				if(startOffice){
					a.flatMap(x => x).
					sortWith(_.result.finalEndTime < _.result.finalEndTime).
					map(Vector(_))
				} else {
					a.map(_.sortWith(_.result.finalEndTime < _.result.finalEndTime)).
					sortWith(_(0).result.finalEndTime < _(0).result.finalEndTime)
				}
			}
		RouteSearchResult(startPoint, endPoint, time1, time2, next)
	}

	def removeLocalTrain(results: IndexedSeq[IndexedSeq[NextResult]]):
		IndexedSeq[IndexedSeq[NextResult]] =
	{
		results.map(r => {
			var last: TrainTime = r(0).link.startTime;
			var ret = Vector[NextResult](r(0));
			r.tail.foreach(n => {
				if(n.link.startTime >= last){
					last = n.link.startTime;
					ret = ret :+ n;
				}
			});
			ret
		})
	}

	case class NextResult (
		link: RouteLink,
		result: RouteSearchResult
	);

}
*/
