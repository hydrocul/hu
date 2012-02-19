package hydrocul.hv.rosen;

case class TrainStationTimeTable(startStation: String, endStation: String,
  table: Seq[(TrainTimePair, String)]){

  def statioinPair = TrainStationPair(startStation, endStation);

}



