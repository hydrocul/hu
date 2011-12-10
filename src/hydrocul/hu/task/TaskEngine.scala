package hydrocul.hu.task;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import scala.annotation.tailrec;

/**
 * タスクを非同期にマルチスレッドに実行するエンジン。mutable なオブジェクト。
 */
private[hu] class TaskEngine {

  def addTask(task: Task){
    val r = task.sync match {
      case Some(consumer) => createConsumerRunnable(task.func, consumer);
      case None => createRunnable(task.func);
    }
    taskQueue.add(r);
    createThreadIfShort();
  }

  def addScheduledTask(delay: TimeOut, task: Task){
    executor.schedule(new Runnable(){
      override def run(){
        addTask(task);
      }
    }, delay.delay, delay.timeunit);
  }

  private val taskQueue = new LinkedBlockingQueue[Runnable]();

  private val maxThreadCount = {
    val p = Runtime.getRuntime.availableProcessors;
    if(p <= 2){
      8;
    } else {
      p * 4;
    }
  }

  // 稼働中のスレッドと起動準備中のスレッドの数
  private val runningThreadCount = new AtomicInteger(0);

  private val executor = new ScheduledThreadPoolExecutor(1); // TODO スレッドがずっと残り続けてしまう

  @tailrec
  private def createThreadIfShort(){
    val c = runningThreadCount.get;
    if(c < maxThreadCount){
      if(!runningThreadCount.compareAndSet(c, c + 1)){
        createThreadIfShort();
      } else {
        val runnable = new Runnable(){
          override def run(){
            try {
              mainLoop();
            } catch { case e =>
              e.printStackTrace(); // TODO
            } finally try {
              shutdownThread();
            } catch { case e =>
              e.printStackTrace(); // TODO
            }
          }
        }
        val thread = new Thread(runnable);
        thread.start();
      }
    }
  }

  private def shutdownThread(){
    runningThreadCount.decrementAndGet();
  }

  private def mainLoop(){
    doTaskLoop(taskQueue, 30);
  }

  private def createRunnable(func: ()=>Unit): Runnable = {
    new Runnable(){
      override def run(){
        func();
      }
    };
  }

  private def createConsumerRunnable(func: ()=>Unit, consumer: Synchronizer): Runnable = {

    consumer.queue.add(createRunnable(func));

    new Runnable(){
      override def run(){
        if(consumer.executing.compareAndSet(false, true)){
          try {
            doTaskLoop(consumer.queue, 0);
          } finally {
            consumer.executing.set(false);
          }
        }
      }
    }

  }

  private def doTaskLoop(queue: LinkedBlockingQueue[Runnable], timeOut: Long){
    @tailrec
    def sub(c: Int){
      val q = queue.poll(timeOut, TimeUnit.MILLISECONDS);
      if(q != null){
        try {
          q.run();
        } catch { case e =>
          e.printStackTrace(); // TODO
        }
        sub(c + 1);
      }
    }
    sub(0);
  }

}

