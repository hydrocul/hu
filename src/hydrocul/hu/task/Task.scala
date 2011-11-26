package hydrocul.hu.task;

private[hu] case class Task(func: ()=>Unit, sync: Option[Synchronizer]);

