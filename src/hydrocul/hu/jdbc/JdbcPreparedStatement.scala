package hydrocul.hu.jdbc;

import hydrocul.hu.IO;

trait JdbcPreparedStatement {

  def execute(args: Seq[Any]): IO[Unit];

  def usingResult[A](args: Seq[Any])(p: Stream[Map[Symbol, Any]] => A): IO[A];

}

private class JdbcPreparedStatementImpl(st: java.sql.PreparedStatement)
  extends JdbcPreparedStatement with Jdbc.Resource {

  def execute(args: Seq[Any]): IO[Unit] = {
    setupArgs(args) >>= { u =>
      IO()(st.executeUpdate());
    }
  }

  def usingResult[A](args: Seq[Any])(p: Stream[Map[Symbol, Any]] => A): IO[A] = {
    setupArgs(args) >>== { u =>
      val rs = st.executeQuery();
      new JdbcResultSetImpl(rs);
    } >>= { rs =>
      Jdbc.resourceUsing(rs){ rs =>
        IO()(p(rs.result));
      }
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

