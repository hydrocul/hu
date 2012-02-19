package hydrocul.hv.rosen;

@SerialVersionUID(7469088663321773385L)
case class TrainTimePair (
	start: TrainTime,
	end: TrainTime
);

object TrainTimePair {

	def apply(start: String, end: String): TrainTimePair =
    new TrainTimePair(TrainTime(start), TrainTime(end));

}
