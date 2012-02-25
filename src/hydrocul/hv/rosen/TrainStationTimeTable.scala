package hydrocul.hv.rosen;

@SerialVersionUID(2239653771886423862L)
case class TrainStationTimeTable(startStation: String, endStation: String,
  table: Seq[(TrainTimePair, String)]);



