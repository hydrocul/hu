package hydrocul.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

public class WriterOutputStream extends org.apache.commons.io.output.WriterOutputStream {

    public WriterOutputStream(Writer writer, String encoding){
        super(writer, encoding);
    }

}
