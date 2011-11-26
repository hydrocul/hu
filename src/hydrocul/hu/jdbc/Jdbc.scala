package hydrocul.hu.jdbc;

import hydrocul.hu.IO;
import hydrocul.hu.TestLib;

object Jdbc {

  def using[A](driver: String, connUri: String)(p: JdbcConnection => IO[A]): IO[A] = {
    IO(){
      Class.forName(driver);
      val conn = java.sql.DriverManager.getConnection(connUri);
      new JdbcConnectionImpl(conn);
    } >>= { conn =>
      resourceUsing(conn)(p);
    }
  }

  def usingSqlite[A](filePath: String)(p: JdbcConnection => IO[A]): IO[A] =
    using("org.sqlite.JDBC", "jdbc:sqlite:" + filePath)(p);

  private[jdbc] trait Resource {

    def close(): IO[Unit];

  }

  private[jdbc] def resourceUsing[A, B <: Resource](resource: B)(p: B => IO[A]): IO[A] = {
    try {
      p(resource).toFinally(resource.close());
    } catch { case e =>
      try {
        resource.close();
      } catch { case e =>
        // ignore
      }
      IO[A](){
        throw e;
      }
    }
  }

  def test: IO[Seq[Option[String]]] = {
    import TestLib._;

    val fname = "tmp/test.sqlite";
    val file = new java.io.File(fname);

    usingSqlite(fname){ conn =>
      {
        conn.execute("create table persons (name, age);");
      } >>= IO.sequential(Vector(
        {
          conn.usingStatement("insert into persons (name, age) values(?, ?);"){ st =>
            st.execute(List("Suzuki", 31));
          }
        } >>= { u =>
          IO.sequential(Vector(
            conn.usingStatement("select name, age from persons;"){ st =>
              st.usingResult(List()){ rs: Stream[Map[Symbol, Any]] =>
                List(
                  assertTrue(!rs.isEmpty),
                  assertEquals(List(Map('name -> "Suzuki", 'age -> "31")), rs)
                );
              }
            },
            conn.usingStatement("select name, age from persons where name=?;"){ st =>
              st.usingResult(List("Suzuki")){ rs: Stream[Map[Symbol, Any]] =>
                List(
                  assertTrue(!rs.isEmpty),
                  assertEquals(List(Map('name -> "Suzuki", 'age -> "31")), rs)
                );
              }
            },
            IO[Seq[Option[String]]](){
              List(
                assertTrue(file.exists)
              );
            }
          )) >>== (_.flatten);
        }
      )) >>== (_.flatten);
    } toFinally IO(){
      file.delete();
    }

  }

}

