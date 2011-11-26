package hydrocul.util;

import java.io.IOException;
import java.io.OutputStream;

public class NullOutputStream extends OutputStream {
	
	public NullOutputStream(){
		// nothing
	}
	
	@Override
	public void write(byte[] buf, int off, int len) throws IOException {
		// nothing
	}
	
	@Override
	public void write(int b) throws IOException {
		// nothing
	}
	
	@Override
	public void flush() throws IOException {
		// nothing
	}
	
	@Override
	public void close() throws IOException {
		// nothing
	}
	
}
