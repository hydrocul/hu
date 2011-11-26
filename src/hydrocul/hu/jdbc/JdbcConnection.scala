package hydrocul.hu.jdbc;

import hydrocul.hu.IO;

trait JdbcConnection {

  def execute(sql: String): IO[Unit];

  def usingStatement[A](sql: String)(p: JdbcPreparedStatement => IO[A]): IO[A];

}

private[jdbc] class JdbcConnectionImpl(conn: java.sql.Connection)
  extends JdbcConnection with Jdbc.Resource {

  def execute(sql: String) = IO(){
    val st = conn.createStatement();
    st.execute(sql);
  }

  def usingStatement[A](sql: String)(p: JdbcPreparedStatement => IO[A]): IO[A] = {
    IO(){
      val st = conn.prepareStatement(sql);
      new JdbcPreparedStatementImpl(st);
    } >>= { st =>
      Jdbc.resourceUsing(st)(p);
    }
  }

  def close() = IO(){
    conn.close();
  }

}
