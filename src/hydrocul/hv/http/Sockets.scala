package hydrocul.hv.http;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;

private[http] object Sockets {

  def doGetIO(host: String, url: UrlInfo, cookie: Map[String, String],
      requestHeader: Seq[(String, String)]): Response = {

    val requestBin = Request.createGet(url, cookie, requestHeader);

    val port = url.port.getOrElse(80);
    val inetAddress = InetAddress.getByName(host);

    val socket = new Socket;
    socket.connect(new InetSocketAddress(inetAddress, port));
    try {

      val output = socket.getOutputStream;
      output.write(requestBin);

      val input = socket.getInputStream;
      val response = Response(input);

      response;

    } finally {
      socket.close();
    }

  }

}
