package hydrocul.util;

import java.io.IOException;
import java.io.Writer;

public class SwitchWriter extends Writer {
    
    private Writer _src;
    
    public SwitchWriter(){
        _src = new NullWriter();
    }
    
    public SwitchWriter(Writer src){
        _src = src;
    }
    
    public void setWriter(Writer src){
        synchronized(this){
            if(src==null){
                src = new NullWriter();
            }
            try {
                _src.flush();
            } catch(IOException e){
                // nothing
            }
            _src = src;
        }
    }
    
    @Override
    public void write(char[] buf, int off, int len) throws IOException {
        synchronized(this){
            _src.write(buf, off, len);
        }
    }
    
    @Override
    public void flush() throws IOException {
        synchronized(this){
            _src.flush();
        }
    }
    
    @Override
    public void close() throws IOException {
        // nothing
    }
    
}
