package hydrocul.hu;

import java.{ io => jio }

import hydrocul.util.FileUtil;

trait File {

  def read: IO[Option[Array[Byte]]];

  def write(bin: Option[Array[Byte]]): IO[Unit];

  def isDirectory: IO[Boolean];

  def list: IO[Map[String, File]];

  def getByName(name: String): Option[File];

  def getByPath(path: String): Option[File];

}

object File {

  def apply(path: String): File = FileImpl(new jio.File(path), false);

  def apply(): File = FileImpl(new jio.File("/"), true);

  private case class FileImpl(file: jio.File, traversalEnable: Boolean) extends File {

    override def read = IO[Option[Array[Byte]]](){
      def sub(f: jio.File): Array[Byte] = {
        val p = new jio.BufferedInputStream(new jio.FileInputStream(f));
        val bop = new jio.ByteArrayOutputStream;
        FileUtil.copyStream(p, bop);
        bop.toByteArray;
      }
      if(!file.exists){
        val tmpPath = file.getPath + ".hu.tmp";
        val tmpFile = new jio.File(tmpPath);
        if(!tmpFile.exists){
          None;
        } else {
          tmpFile.renameTo(file);
          Some(sub(file));
        }
      } else {
        Some(sub(file));
      }
    }

    override def write(bin: Option[Array[Byte]]) = IO[Unit](){
      bin match {
        case Some(bin) =>
          val parent = file.getParentFile;
          if(!parent.exists){
            parent.mkdirs();
          }
          val path = file.getPath;
          val tmpPath = path + ".hu.tmp";
          val tmpFile = new jio.File(tmpPath);
          val fp = new jio.FileOutputStream(tmpFile);
          val lock = fp.getChannel.lock;
          val p = new jio.BufferedOutputStream(fp);
          try {
            p.write(bin);
          } finally {
            lock.release();
            p.close();
          }
          if(file.exists && !file.delete()){
            throw new jio.IOException("Can't delete: " + path);
          }
          if(!tmpFile.renameTo(file)){
            throw new jio.IOException("Can't rename: " + path);
          }
        case None =>
          if(file.exists && !file.delete()){
            val path = file.getPath;
            throw new jio.IOException("Can't delete: " + path);
          }
      }
    }

    override def isDirectory = IO[Boolean]()(file.isDirectory);

    override def list = IO[Map[String, File]](){
      val list = file.listFiles;
      if(list == null){
        Map.empty;
      } else {
        list.filter { f =>
          val name = f.getName;
          if(name == "." || name == ".."){
            false;
          } else if(name.endsWith(".hu.tmp")){
            false;
          } else {
            true;
          }
        }.map { f: jio.File =>
          (f.getName, FileImpl(f, traversalEnable));
        }.toMap[String, File];
      }
    }

    override def getByName(name: String): Option[File] = {
      if(name.indexOf('/') >= 0 || name.indexOf('\\') >= 0){
        None;
      } else if(name == "."){
        if(traversalEnable){
          Some(this);
        } else {
          None;
        }
      } else if(name == ".."){
        if(traversalEnable){
          val parent = file.getParentFile;
          if(parent == null){
            None;
          } else {
            Some(FileImpl(parent, traversalEnable));
          }
        } else {
          None;
        }
      } else {
        Some(FileImpl(new jio.File(file, name), traversalEnable));
      }
    }

    override def getByPath(path: String): Option[File] = {
      val p1 = path.indexOf('/');
      val p2 = path.indexOf('\\');
      val p = if(p1 >= 0 && p2 >= 0){
        p1 min p2;
      } else if(p1 >= 0){
        p1;
      } else if(p2 >= 0){
        p2;
      } else {
        -1;
      }
      if(p==0){
        if(traversalEnable){
          Some(FileImpl(new jio.File(path), traversalEnable));
        } else {
          None;
        }
      } else if(p > 0){
        val name = path.substring(0, p);
        val tail = path.substring(p + 1);
        if(tail.isEmpty){
          getByName(name);
        } else {
          getByName(name) match {
            case Some(f) => f.getByPath(tail);
            case None => None;
          }
        }
      } else {
        getByName(path);
      }
    }

  }

}
