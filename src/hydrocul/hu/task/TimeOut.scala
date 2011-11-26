package hydrocul.hu.task;

import java.util.concurrent.TimeUnit;

case class TimeOut(delay: Long, timeunit: TimeUnit);

object TimeOut {

  def apply(delay: Long): TimeOut = TimeOut(delay.asInstanceOf[Long], TimeUnit.MILLISECONDS);

  def apply(delay: Double): TimeOut = TimeOut(delay.asInstanceOf[Long], TimeUnit.MILLISECONDS);

}
