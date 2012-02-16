package hydrocul.hv.rosen.ekikara;

import hydrocul.hv.http.Page;
import hydrocul.hv.rosen.TrainTimePair;

object EkikaraTableScraper {

  def scrape(url: String, startEndList: List[StartEndIndex],
    prevResult: List[IndexedSeq[TrainTimePair]],
    doGet: String => Page){
/* TODO
		val doc = content.getDOM;

 		val table = doc.getElementById("container02").getChildNodes.
			filter(_.getTagName()=="TABLE")(7).
			getElementsByTagName("TABLE")(0).
			getElementsByTagName("TABLE")(0);
		val trList = table.getElementsByTagName("TR");
		val tdList: IndexedSeq[IndexedSeq[Option[TrainTime]]] =
			(6 until trList.length - 1).map(index => {
				val tr = trList(index);
				val tdList = tr.getElementsByTagName("TD");
				tdList.drop(2).map(td =>
					td.getElementsByTagName("SPAN").
						map(s => TrainTime(s.getTextContent.trim)))
			}).transpose.map(_.flatten);
		val list: List[IndexedSeq[TrainTimePair]] = startEndList.map(
			startEnd => {
				tdList.map(td => {
					(td(startEnd.startIndex), td(startEnd.endIndex))
				}).flatMap(tt => {
					if(!tt._1.isDefined || !tt._2.isDefined){
						Nil
					} else {
						TrainTimePair(tt._1.get, tt._2.get) :: Nil
					}
				})
			}
		);
		val result = prevResult.zip(list).map(r => r._1 ++ r._2);
		val aList = doc.getElementById("container02").getChildNodes.
			filter(_.getTagName()=="TABLE")(6).
			getElementsByTagName("SPAN")(0).
			getElementsByTagName("A").
			filter(_.getTextContent=="次頁");
		if(aList.length > 0){
			val fname = aList(0).getAttribute("href");
			val p = url.lastIndexOf('/');
			val nextUrl = url.substring(0, p) + "/" + fname;
			FetchUrl.task(nextUrl, scrape(_, nextUrl, startEndList,
					result, filter, task)).submit();
		} else {
			DBObject.getAndPut[TrainStationTimeTable](
					"hydrocul.dog.rosen.timeTable")(mapOp => {
 				var map2 = mapOp match {
					case Some(map) => map
					case None => Map.empty[TrainStationPair, IndexedSeq[TrainTimePair]]
				}
				startEndList.map(s => TrainStationPair(s.startStation, s.endStation)).
						zip(result).foreach({
					case (stationPair, times) => {
						map2 = map2 + filter(map2, stationPair, times);
					}
				});
				map2
			});
			task.schedule(6, TimeUnit.HOURS);
		}
*/
  }

	case class StartEndIndex (
		startStation: String,
		endStation: String,
		startIndex: Int,
		endIndex: Int
	);

}
