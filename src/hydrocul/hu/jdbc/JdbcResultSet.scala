package hydrocul.hu.jdbc;

import hydrocul.hu.IO;

private[jdbc] class JdbcResultSetImpl(rs: java.sql.ResultSet) extends Jdbc.Resource {

  val result: Stream[Map[Symbol, Any]] = {
    val meta = rs.getMetaData;
    val meta2 = (0 until meta.getColumnCount).map { index =>
      Symbol(meta.getColumnLabel(index + 1));
    }
    createResultSub(meta2);
  }

  def close() = IO(){
    rs.close();
  }

  private[this] def createResultSub(meta: IndexedSeq[Symbol]): Stream[Map[Symbol, Any]] = {
    if(rs.next()){
      val firstRecord: Map[Symbol, Any] = {
        (0 until meta.size).map { index =>
          val value = rs.getString(index + 1); // TODO 型別に処理すべき
          (meta(index), value);
        }.toMap;
      }
      Stream.cons(firstRecord, createResultSub(meta));
    } else {
      Stream.empty;
    }
  }

}

