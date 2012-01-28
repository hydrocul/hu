package hydrocul.hv.http;

import java.{ io => jio }
import java.{ net => jnet }

private[http] object Sockets {

  def doGet(host: String, url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): Response = {

    val scheme = url.scheme;

    val requestBin = Request.createGet(url, cookie, requestHeader);

    val port = url.port.getOrElse(80);
    val inetAddress = jnet.InetAddress.getByName(host);

    socketUsing(scheme, inetAddress, port){ socket =>

      val output = socket.outputStream;
      output.write(requestBin);
      output.flush();

      val input = socket.inputStream;
/*
if(scheme=="https"){
val ip = input;
while({
  val c = ip.read();
  if(c < 0){
    false;
  } else {
    print(c.asInstanceOf[Char]);
    true;
  }
}){}
}
*/
      val response = Response(input);

      response;

    }

  }

  private trait Socket {

    def outputStream: jio.OutputStream;

    def inputStream: jio.InputStream;

  }

  private def socketUsing[A](scheme: String, inetAddress: jnet.InetAddress,
    port: Int)(p: Socket => A): A = {

    scheme match {
      case "http" =>
        normalSocketUsing(inetAddress, port)(p);
      case "https" =>
        sslSocketUsing(inetAddress, port)(p);
      case _ =>
        throw new Exception("Unknown sheme: " + scheme);
    }

  }

  private def normalSocketUsing[A](inetAddress: jnet.InetAddress,
    port: Int)(p: Socket => A): A = {

    val socket = new jnet.Socket();
    socket.connect(new jnet.InetSocketAddress(inetAddress, port));
    val socket2 = new Socket {
      def outputStream: jio.OutputStream = socket.getOutputStream();
      def inputStream: jio.InputStream = socket.getInputStream();
    }
    try {
      p(socket2);
    } finally {
      socket.close();
    }

  }

  private def sslSocketUsing[A](inetAddress: jnet.InetAddress,
    port: Int)(p: Socket => A): A = {

    val cacertsPath = {
      {
        val javaHome = System.getenv("JAVA_HOME");
        if(javaHome == null){
          None;
        } else {
          val path = javaHome + "/jre/lib/security/cacerts";
          if((new jio.File(path)).exists){
            Some(path);
          } else {
            None;
          }
        }
      } orElse {
        val path = "/etc/ssl/certs/java/cacerts";
        if((new jio.File(path)).exists){
          Some(path);
        } else {
          None;
        }
      } getOrElse {
        throw new Exception("Not Found cacerts");
      }
    }
    val cacertsPw = "changeit";

    val keyStore = java.security.KeyStore.getInstance("JKS");
    keyStore.load(new java.io.FileInputStream(cacertsPath), cacertsPw.toCharArray);

    val tmf = javax.net.ssl.TrustManagerFactory.getInstance("PKIX");
    tmf.init(keyStore);

    val context = javax.net.ssl.SSLContext.getInstance("TLS");
    context.init(null, tmf.getTrustManagers(), null);

    val sf = context.getSocketFactory();
    val socket = sf.createSocket(inetAddress, 443).asInstanceOf[javax.net.ssl.SSLSocket];
    socket.startHandshake();

    val socket2 = new Socket {
      def outputStream: jio.OutputStream = socket.getOutputStream();
      def inputStream: jio.InputStream = socket.getInputStream();
    }
    try {
      p(socket2);
    } finally {
      socket.close();
    }

  }

}
