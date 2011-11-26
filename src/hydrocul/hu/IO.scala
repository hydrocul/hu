package hydrocul.hu;

import hydrocul.hu.{ task => taskmanager }

final class IO[+A] private (private val task: (Either[Throwable, A] => Unit) => Unit) {

  def >>= [B](p: A => IO[B]): IO[B] = new IO[B]({ p2: (Either[Throwable, B] => Unit) =>
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

  def >>= [A1, A2, B](p: (A1, A2) => IO[B])(implicit ev: A <:< (A1, A2)): IO[B] =
    this >>= { a => p.tupled(ev(a)); }

  def >>= [A1, A2, A3, B](p: (A1, A2, A3) => IO[B])(implicit ev: A <:< (A1, A2, A3)): IO[B] =
    this >>= { a => p.tupled(ev(a)); }

  def >>= [B](io: => IO[B])(implicit ev: A <:< Unit): IO[B] =
    this >>= { u: A => io; }

  def >>== [B](p: A => B): IO[B] = new IO[B]({ p2: (Either[Throwable, B] => Unit) =>
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

  def >>== [A1, A2, B](p: (A1, A2) => B)(implicit ev: A <:< (A1, A2)): IO[B] =
    this >>== { a => p.tupled(ev(a)); }

  def >>== [A1, A2, A3, B](p: (A1, A2, A3) => B)(implicit ev: A <:< (A1, A2, A3)): IO[B] =
    this >>== { a => p.tupled(ev(a)); }

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

  def toFinally(p: IO[Unit]): IO[A] = {
    toEither >>= { a: Either[Throwable, A] =>
      p >>== { u => a; }
    } toThrowable;
  }

  def exec(){
    task { r =>
      r match {
        case Right(r) =>
          if(!r.isInstanceOf[Unit]){
            println(r);
          }
        case Left(e) =>
          e.printStackTrace();
      }
    }
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
        IO()(result);
      } else {
        list.head >>= { a =>
          sub(list.tail, a :: result);
        }
      }
    }
    sub(list, Nil) >>== { l: List[A] =>
      l.toIndexedSeq;
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

  private def test(all: Boolean){

    val test2: Seq[IO[Seq[Option[String]]]] = List(
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
