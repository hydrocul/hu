package hydrocul.util;

import java.io.IOException;
import java.io.Writer;

public class NullWriter extends Writer {
	
	public NullWriter(){
		// nothing
	}
	
	@Override
	public void write(char[] buf, int off, int len) throws IOException {
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
