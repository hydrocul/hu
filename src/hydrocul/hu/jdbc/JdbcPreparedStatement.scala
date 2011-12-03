package hydrocul.hu.jdbc;

import hydrocul.hu.IO;

trait JdbcPreparedStatement {

  def execute(args: Seq[Any]): IO[Unit];

  def usingResult[A](args: Seq[Any])(p: Stream[Map[Symbol, Any]] => A): IO[A];

}

private class JdbcPreparedStatementImpl(st: java.sql.PreparedStatement, sql: String)
  extends JdbcPreparedStatement with Jdbc.Resource {

  def execute(args: Seq[Any]): IO[Unit] = {
    try {
      setupArgs(args) >>= { u =>
        IO()(try {
          st.executeUpdate();
        } catch { case e =>
          throw new Exception("sql: " + sql + " args: " + args.toString, e);
        });
      }
    } catch { case e =>
      throw new Exception("sql: " + sql + " args: " + args.toString, e);
    }
  }

  def usingResult[A](args: Seq[Any])(p: Stream[Map[Symbol, Any]] => A): IO[A] = {
    try {
      setupArgs(args) >>== { u =>
        try {
          val rs = st.executeQuery();
          new JdbcResultSetImpl(rs);
        } catch { case e =>
          throw new Exception("sql: " + sql + " args: " + args.toString, e);
        }
      } >>= { rs =>
        Jdbc.resourceUsing(rs){ rs =>
          IO()(p(rs.result));
        }
      }
    } catch { case e =>
      throw new Exception("sql: " + sql + " args: " + args.toString, e);
    }
  }

  def close() = IO(){
    st.close();
  }

  private[this] def setupArgs(args: Seq[Any]): IO[Unit] = {
    setupArgsSub(1, args);
  }

  private[this] def setupArgsSub(index: Int, args: Seq[Any]): IO[Unit] = {
    if(args.isEmpty){
      IO()(());
    } else {
      IO(){
        args.head match {
          case a: String =>
            st.setString(index, a);
          case a =>
            st.setString(index, a.toString); // TODO 型別に処理すべき
        }
      } >>=
      { _ =>
        setupArgsSub(index + 1, args.tail);
      }
    }
  }

}

