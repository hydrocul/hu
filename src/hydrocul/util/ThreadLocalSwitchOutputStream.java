package hydrocul.util;

import java.io.IOException;
import java.io.OutputStream;

public class ThreadLocalSwitchOutputStream extends OutputStream {

	private final OutputStream _default;
	private final ThreadLocal<OutputStream> _src;
	
	public ThreadLocalSwitchOutputStream(OutputStream defaultOutputStream){
		_default = defaultOutputStream;
		_src = new ThreadLocal<OutputStream>(){
			@Override
			protected OutputStream initialValue(){
				return _default;
			}
		};
	}
	
	public OutputStream getOutputStream(){
		return _src.get();
	}

	public void setOutputStream(OutputStream outputStream){
		if(outputStream==null){
 			outputStream = _default;
		}
		try {
			OutputStream oldOutputStream = _src.get();
			if(oldOutputStream!=null){
				oldOutputStream.flush();
			}
		} catch(IOException e){
			// nothing
		}
		_src.set(outputStream);
	}
	
	@Override
	public void write(byte[] buf, int off, int len) throws IOException {
		OutputStream outputStream = _src.get();
		if(outputStream!=null){
			outputStream.write(buf, off, len);
			outputStream.flush();
		}
	}
	
	@Override
	public void write(int b) throws IOException {
		OutputStream outputStream = _src.get();
		if(outputStream!=null){
			outputStream.write(b);
			outputStream.flush();
		}
	}
	
	@Override
	public void flush() throws IOException {
		OutputStream outputStream = _src.get();
		if(outputStream!=null){
			outputStream.flush();
		}
	}
	
	@Override
	public void close() throws IOException {
		OutputStream outputStream = _src.get();
		if(outputStream!=null){
			outputStream.close();
			_src.set(null);
		}
	}
	
}
