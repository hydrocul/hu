package hydrocul.hu.task;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

class Synchronizer {

  private[task] val queue = new LinkedBlockingQueue[Runnable]();

  private[task] val executing = new AtomicBoolean(false);

}

