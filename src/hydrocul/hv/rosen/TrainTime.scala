package hydrocul.hv.rosen;

@SerialVersionUID(1081878479020877548L)
case class TrainTime (
	h: Int,
	m: Int
) extends Ordered[TrainTime] {

	override def toString: String = "%02d:%02d".format(h, m);

	override def compare(that: TrainTime): Int = mm - that.mm;

	def mm = if(h < 4) ( h + 24 ) * 60 + m else h * 60 + m;

	def + (d: Int): TrainTime = TrainTime(mm + d);

	def - (d: Int): TrainTime = TrainTime(mm - d);

	def - (that: TrainTime): Int = mm - that.mm;

}

object TrainTime {

	def apply(str: String): TrainTime = {
		str match {
			case TrainTimeFormat(h, m) => TrainTime(h.toInt, m.toInt)
			case _ => throw new IllegalArgumentException(str);
		}
	}

	def apply(a: Int): TrainTime = {
		val c = if(a < 0) a % ( 24 * 60 ) + ( 24 * 60 ) else a;
		val h = c / 60;
		val m = c % 60;
		TrainTime(h % 24, m)
	}

	def now: TrainTime = {
		val date = new java.util.GregorianCalendar();
		val h = date.get(java.util.Calendar.HOUR_OF_DAY);
		val m = date.get(java.util.Calendar.MINUTE);
		TrainTime(h, m) + 1
	}

	private val TrainTimeFormat = "(\\d\\d):(\\d\\d)".r;

}

