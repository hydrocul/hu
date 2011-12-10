package hydrocul.hu;

import scala.collection.immutable.Queue;

import hydrocul.hu.{ task => taskmanager }

final class IO[+A] private (private val task: (Either[Throwable, A] => Unit) => Unit) {

  def flatMap[B](p: A => IO[B]): IO[B] = new IO[B]({ p2: (Either[Throwable, B] => Unit) =>
    task { a: Either[Throwable, A] =>
      a match {
        case Right(a) =>
          IO.taskEngine.addTask(new taskmanager.Task({ () =>
            try {
              p(a).task(p2);
            } catch { case e =>
              p2(Left(e));
            }
          }, None));
        case Left(e) =>
          p2(Left(e));
      }
    }
  });

  // @deprecated("use map", "flatMap")
  def >>= [B](p: A => IO[B]): IO[B] = flatMap(p);

  def map[B](p: A => B): IO[B] = new IO[B]({ p2: (Either[Throwable, B] => Unit) =>
    task { a: Either[Throwable, A] =>
      a match {
        case Right(a) =>
          IO.taskEngine.addTask(new taskmanager.Task({ () =>
            try {
              p2(Right((p(a))));
            } catch { case e =>
              p2(Left(e));
            }
          }, None));
        case Left(e) =>
          p2(Left(e));
      }
    }
  });

  @deprecated("use map", "")
  def >>== [B](p: A => B): IO[B] = map(p);

  def filter(p: A => Boolean): IO[A] = new IO[A](
      { p2: (Either[Throwable, A] => Unit) =>
    task { a: Either[Throwable, A] =>
      a match {
        case Right(a) =>
          if(p(a)){
            p2(Right(a));
          } else {
            p2(Left(new MatchError(a)));
          }
        case Left(e) =>
          p2(Left(e));
      }
    }
  });

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(io => ev(io));

  def toEither: IO[Either[Throwable, A]] = new IO[Either[Throwable, A]](
      { p2: (Either[Throwable, Either[Throwable, A]] => Unit) =>
    task { a: Either[Throwable, A] =>
      p2(Right(a));
    }
  });

  def toThrowable[B](implicit ev: A <:< Either[Throwable, B]): IO[B] = new IO[B](
      { p2: (Either[Throwable, B] => Unit) =>
    task { a: Either[Throwable, A] =>
      a match {
        case Right(a) =>
          p2(ev(a));
        case Left(e) =>
          p2(Left(e));
      }
    }
  });

  @deprecated("use iotry and iofinally", "")
  def toFinally(p: IO[Unit]): IO[A] = {
    toEither >>= { a: Either[Throwable, A] =>
      p.map { u => a; }
    } toThrowable;
  }

  def then[B](p: A => IO[B]): IO[B] = flatMap(p);

  def iotry[B](p: A => IO[B]) = new {

    def iofinally(p2: A => IO[Unit]): IO[B] = {
      toEither.flatMap {
        case Right(a) =>
          p(a).toEither.flatMap {
            case Right(a2) =>
              p2(a).map { _ => Right(a2); }
            case Left(e) =>
              p2(a).map { _ => Left(e); }
          }
        case Left(e) =>
          IO()(Left(e));
      } toThrowable;
    }

  }

  def thru(p: A => IO[Unit]): IO[A] = {
    flatMap { a: A =>
      p(a).map(_ => a);
    }
  }

  def toUnit(implicit ev: A <:< Seq[Unit]): IO[Unit] = map(_ => ());

  def exec(): A = {
    val a = new Object();
    @volatile var ret: Any = null;
    task { r =>
      r match {
        case Right(r) =>
          if(!r.isInstanceOf[Unit]){
            println(r);
            ret = r;
          }
        case Left(e) =>
          e.printStackTrace();
      }
      a.synchronized {
        a.notifyAll();
      }
    }
    a.synchronized {
      a.wait();
    }
    ret.asInstanceOf[A];
  }

}

object IO {

  def apply[A]()(delayed: =>A): IO[A] = new IO[A]({ p: (Either[Throwable, A] => Unit) =>
    p(Right(delayed));
  });

  def apply[A](sync: taskmanager.Synchronizer)(delayed: =>A): IO[A] = {
    new IO[A]({ p: (Either[Throwable, A] => Unit) =>
      taskEngine.addTask(new taskmanager.Task({() =>
        try {
          val d = delayed;
          taskEngine.addTask(new taskmanager.Task({() =>
            try {
              p(Right(d));
            } catch { case e =>
              p(Left(e));
            }
          }, None));
        } catch { case e =>
          p(Left(e));
        }
      }, Some(sync)));
    });
  }

  def sleep(delay: taskmanager.TimeOut): IO[Unit] = {
    new IO[Unit]({ p: (Either[Throwable, Unit] => Unit) =>
      taskEngine.addScheduledTask(delay, new taskmanager.Task({() =>
        try {
          p(Right(()));
        } catch { case e =>
          p(Left(e));
        }
      }, None));
    });
  }

  def sequential[A](list: Seq[IO[A]]): IO[Seq[A]] = {
    def sub[A](list: Seq[IO[A]], result: List[A]): IO[List[A]] = {
      if(list.isEmpty){
        IO()(result.reverse);
      } else {
        list.head >>= { a =>
          sub(list.tail, a :: result);
        }
      }
    }
    sub(list, Nil);
  }

  def seq[A](ios: IO[A]*): IO[Seq[A]] = sequential(ios);

  def seqt[A1, A2](a1: IO[A1], a2: IO[A2]): IO[(A1, A2)] = {
    sequential(Vector(a1, a2)) map { v =>
      (v(0).asInstanceOf[A1], v(1).asInstanceOf[A2]);
    }
  }

  def parallel[A](list: IndexedSeq[IO[A]]): IO[IndexedSeq[A]] = {
    val sync = new taskmanager.Synchronizer;
    @volatile var count: Int = list.size;
    @volatile var resultList: IndexedSeq[Option[A]] = Vector.fill[Option[A]](list.size)(None);
    def resultTask(index: Int, result: Either[Throwable, A],
        p: Either[Throwable, IndexedSeq[A]] => Unit) = {
      result match {
        case Left(e) =>
          new taskmanager.Task({() =>
            count = 0;
            taskEngine.addTask(new taskmanager.Task({() =>
              p(Left(e));
            }, None));
          }, Some(sync));
        case Right(a) =>
          new taskmanager.Task({() =>
            resultList = resultList.updated(index, Some(a));
            count = count - 1;
            if(count == 0){
              taskEngine.addTask(new taskmanager.Task({() =>
                try {
                  p(Right(resultList.map(_.get)));
                } catch { case e =>
                  p(Left(e));
                }
              }, None));
            }
          }, Some(sync));
      }
    }
    new IO[IndexedSeq[A]]({ p: (Either[Throwable, IndexedSeq[A]] => Unit) =>
      (0 until list.size).foreach { i =>
        list(i).task { a: Either[Throwable, A] =>
          taskEngine.addTask(resultTask(i, a, p));
        }
      }
    });
  }

  def pipe[A]: (IO[A], (Either[Throwable, A] => IO[Unit])) = {

    val sync = new Object;
    var value: Either[Throwable, A] = null;
    var receiver: (Either[Throwable, A] => Unit) = null;

    def thrue(){
      val v = value;
      val r = receiver;
      taskEngine.addTask(new taskmanager.Task({() =>
        try {
          r(v);
        } catch { case e =>
          r(Left(e));
        }
      }, None));
      value = null;
      receiver = null;
    }

    val io = new IO[A]({ p: (Either[Throwable, A] => Unit) =>
      sync.synchronized {
        receiver = p;
        if(value != null){
          thrue();
        }
      }
    })

    val sender = { a: Either[Throwable, A] => IO(){
      sync.synchronized {
        value = a;
        if(receiver != null){
          thrue();
        }
      }
    } }

    (io, sender);

  }

  trait Mailbox[A]{

    def send(msg: A): IO[Unit];

    def receive: IO[A];

  }

  def mailbox[A]: Mailbox[A] = new Mailbox[A]{

    def send(msg: A): IO[Unit] = {
      IO[IO[Unit]](){
        sync.synchronized {
          if(queue2.isEmpty){
            val t = pipe[A];
            queue1.enqueue(t._1);
            t._2.apply(Right(msg));
          } else {
            val t = queue2.dequeue;
            queue2 = t._2;
            t._1.apply(Right(msg));
          }
        }
      } flatten;
    }

    def receive: IO[A] = {
      IO[IO[A]](){
        sync.synchronized {
          if(queue1.isEmpty){
            val t = pipe[A];
            queue2 = queue2.enqueue(t._2);
            t._1;
          } else {
            val t = queue1.dequeue;
            queue1 = t._2;
            t._1;
          }
        }
      } flatten;
    }

    private val sync = new Object;
    private var queue1 = Queue[IO[A]]();
    private var queue2 = Queue[Either[Throwable, A] => IO[Unit]]();

  }

  def main(args: Array[String]){
    main(args.toList);
  }

  private def main(args: List[String]){
    args match {
      case "test" :: "all" :: Nil =>
        test(true);
      case "test" :: Nil =>
        test(false);
      case _ =>
        throw new IllegalArgumentException();
    }
  }

  private def iotest: IO[Seq[Option[String]]] = {
    import TestLib._;
    val io1 = IO.sequential(Vector(IO()(1), IO()(2), IO()(3))).map { r: Seq[Int] =>
      List(
        assertEquals(Vector(1, 2, 3), r)
      );
    }
    val io2 = IO.parallel(Vector(IO()(1), IO()(2), IO()(3))).map { r: Seq[Int] =>
      List(
        assertEquals(Vector(1, 2, 3), r)
      );
    }
    val io3 = {
      val t = pipe[Int];
      t._2.apply(Right(3)) >>= { _ =>
        t._1 map { r =>
          List(
            assertEquals(3, r)
          )
        }
      }
    }
    val io4 = {
      val t = pipe[Int];
      IO.parallel(Vector(
        sleep(taskmanager.TimeOut(1000)) >>= { _ =>
          t._2.apply(Right(4));
        },
        t._1
      )) map { v =>
        List(
          assertEquals(Vector((), 4), v)
        )
      }
    }
    val io5 = {
      def f() = IO()((1, 3));
      def g(a: Int, b: Int) = IO()(a + b);
      def h(i: Int) = IO()(
        List(
          assertEquals(4, i)
        )
      );
      for {
        (a, b) <- f()
        r <- g(a, b)
        s <- h(r)
      } yield s;
    }
    val io6 = {
      IO()(5) then {
        i => IO()(i * 2);
      } iotry {
        i => IO()(List(assertEquals(10, i)));
      } iofinally {
        _ => IO()();
      }
    }
    for {
      r1 <- io1
      r2 <- io2
      r3 <- io3
      r4 <- io4
      r5 <- io5
      r6 <- io6
    } yield r1 ++ r2 ++ r3 ++ r4 ++ r5 ++ r6;
  }

  private def test(all: Boolean){

    val test2: Seq[IO[Seq[Option[String]]]] = List(
      iotest,
      IO()(UrlUtil.test),
      IO()(CsvParser.test),
      jdbc.Jdbc.test
    );

    val sync = new taskmanager.Synchronizer;

    val test3 = test2.map { io =>
      io >>= { r: Seq[Option[String]] =>
        IO[(Int, Int)](sync){
          val f = r.filter(_.isDefined).map(_.get);
          f.foreach { l: String =>
            println(l);
          }
          (r.size - f.size, f.size);
        }
      }
    }

    parallel(test3.toIndexedSeq).task { r: Either[Throwable, IndexedSeq[(Int, Int)]] =>
      r match {
        case Right(r) =>
          val success = r.map(_._1).sum;
          val failed = r.map(_._2).sum;
          if(failed > 0){
            println("Failed: %d / %d".format(success, success + failed));
            sys.exit(1);
          } else {
            println("Success: %d / %d".format(success, success + failed));
            sys.exit(0);
          }
        case Left(e) =>
          e.printStackTrace();
          sys.exit(1);
      }
    }

  }

  private val taskEngine = new taskmanager.TaskEngine;

}
