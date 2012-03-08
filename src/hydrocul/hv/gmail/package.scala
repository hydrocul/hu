package hydrocul.hv;

import java.{ io => jio }
import java.util.Properties;
import javax.mail.Authenticator;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.Message.RecipientType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

package object gmail {

  def sendmail(to: String, subject: String, body: String,
      gmailAddress: String, gmailPasswd: String){
    sendmailSub(to, subject, gmailAddress, gmailPasswd,
            { mimeMessage: javax.mail.internet.MimeMessage =>
              mimeMessage.setText(body, "ISO-2022-JP");
            });
  }

  def sendHtmlMail(to: String, subject: String, html: String,
      attachments: Seq[HtmlAttachment],
      gmailAddress: String, gmailPasswd: String): Unit = {
    sendmailSub(to, subject, gmailAddress, gmailPasswd,
                { mimeMessage: javax.mail.internet.MimeMessage =>
      if(attachments.isEmpty){
        mimeMessage.setContent(html, "text/html; charset=UTF-8");
      } else {
        val multipart = new javax.mail.internet.MimeMultipart;
        multipart.setSubType("related");
        {
          val bodyPart = new javax.mail.internet.MimeBodyPart();
          bodyPart.setContent(html, "text/html; charset=UTF-8");
          multipart.addBodyPart(bodyPart);
        };
        attachments.foreach { attachment =>
          val bodyPart = new javax.mail.internet.MimeBodyPart();
          val dataHandler = new javax.activation.DataHandler(
              new javax.activation.DataSource(){
            def getContentType(): String =
              attachment.contentType;
            def getInputStream(): jio.InputStream =
              new jio.ByteArrayInputStream(attachment.content);
            def getName(): String =
              attachment.filename;
            def getOutputStream(): jio.OutputStream =
              throw new UnsupportedOperationException();
          });
          bodyPart.setDataHandler(dataHandler);
          bodyPart.setFileName(attachment.filename);
          bodyPart.setDisposition(javax.mail.Part.INLINE);
          bodyPart.setContentID(attachment.filename);
          multipart.addBodyPart(bodyPart);
        }
        mimeMessage.setContent(multipart);
      }
    });
  }

  private def sendmailSub(to: String, subject: String,
      gmailAddress: String, gmailPasswd: String,
      messageBuilder: MimeMessage => Unit){

    val session : javax.mail.Session = {

      java.security.Security.addProvider(
        new com.sun.net.ssl.internal.ssl.Provider());
      val SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";

      val props = System.getProperties();
      props.setProperty("mail.smtp.host", "smtp.gmail.com");
      props.setProperty("mail.smtp.socketFactory.class", SSL_FACTORY);
      props.setProperty("mail.smtp.socketFactory.fallback", "false");
      props.setProperty("mail.smtp.port", "465");
      props.setProperty("mail.smtp.socketFactory.port", "465");
      props.setProperty("mail.smtp.auth", "true");

      javax.mail.Session.getInstance(props,
          new javax.mail.Authenticator(){
        override protected def getPasswordAuthentication():
            javax.mail.PasswordAuthentication = {
          new javax.mail.PasswordAuthentication(gmailAddress,
              gmailPasswd);
        }
      });

    }
    val mimeMessage = new javax.mail.internet.MimeMessage(session);
    mimeMessage.setFrom(new javax.mail.internet.InternetAddress(gmailAddress));
    mimeMessage.setRecipients(javax.mail.Message.RecipientType.TO, to);
    mimeMessage.setSubject(subject, "ISO-2022-JP");
    mimeMessage.setSentDate(new java.util.Date());
    messageBuilder(mimeMessage);
    javax.mail.Transport.send(mimeMessage);

  }

  def receivePop3(gmailAddress: String, gmailPassword: String): Seq[Pop3Mail] = {

    val props = new Properties;

    props.setProperty("mail.pop3.host", "pop.gmail.com");
    props.setProperty("mail.pop3.port", "995");

    props.setProperty("mail.pop3.connectiontimeout", "60000");
    props.setProperty("mail.pop3.timeout", "60000");

    props.setProperty("mail.pop3.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
    props.setProperty("mail.pop3.socketFactory.fallback", "false");
    props.setProperty("mail.pop3.socketFactory.port", "995");

    val session = Session.getInstance(props, new Authenticator(){
      override protected def getPasswordAuthentication(): PasswordAuthentication =
        new PasswordAuthentication(gmailAddress, gmailPassword);
    });

    def using[A <: { def close(); }, B](resource: A)(p: A => B): B = {
      val result = try {
        p(resource);
      } catch { case e =>
        try {
          resource.close();
        } catch { case _ => ; } // その前の例外を優先する
        throw e;
      }
      resource.close(); // 正常にpを処理できた場合にcloseを呼び出す
      result;
    }

    val store = session.getStore("pop3");
    store.connect();
    using(store){ store =>

      val folder = store.getFolder("INBOX");
      folder.open(Folder.READ_ONLY);

      using(new { def close(){ folder.close(false); } } ){ _ =>

        val mails = folder.getMessages.map { case message: MimeMessage =>
          val body = message.getContent match {
            case body: String => body;
            case _ => "";
          }
          Pop3Mail(
            message.getMessageID,
            message.getSubject,
            message.getSentDate.toString,
            message.getFrom()(0).asInstanceOf[InternetAddress].getAddress,
            message.getRecipients(RecipientType.TO).
              map(_.asInstanceOf[InternetAddress].getAddress.toString),
            body);
        }

        mails;

      }

    }

  }

}
