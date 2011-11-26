package hydrocul.util;

import java.io.IOException;
import java.io.OutputStream;

public class SwitchOutputStream extends OutputStream {
    
    private OutputStream _src;
    
    public SwitchOutputStream(){
        _src = new NullOutputStream();
    }
    
    public SwitchOutputStream(OutputStream src){
        _src = src;
    }
    
    public void setOutputStream(OutputStream src){
        synchronized(this){
            if(src==null){
                src = new NullOutputStream();
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
    public void write(byte[] buf, int off, int len) throws IOException {
        synchronized(this){
            _src.write(buf, off, len);
        }
    }
    
    @Override
    public void write(int b) throws IOException {
        synchronized(this){
            _src.write(b);
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
