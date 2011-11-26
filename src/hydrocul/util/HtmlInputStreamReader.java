package hydrocul.util;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class HtmlInputStreamReader extends Reader {
	
	private InputStream _src;
	private String _url;
	private StringBuilder _headBuf;
	private Reader _reader;
	
	private byte[] _binBuf;
	private int _len; 
	
	private String _charset;
	
	public HtmlInputStreamReader(InputStream src){
		_src = src;
		_url = null;
		_headBuf = new StringBuilder();
		_reader = null;
		_binBuf = new byte[2048];
		_len = 0;
		_charset = null;
	}
	
	public HtmlInputStreamReader(String url){
		_src = null;
		_url = url;
		_headBuf = new StringBuilder();
		_reader = null;
		_binBuf = new byte[2048];
		_len = 0;
		_charset = null;
	}

	@Override
	public int read() throws IOException {
		initReader();
		return _reader.read();
	}
	
	@Override
	public int read(char[] buf) throws IOException {
		return read(buf, 0, buf.length);
	}
	
	@Override
	public int read(char[] buf, int off, int len) throws IOException {
		initReader();
		return _reader.read(buf, off, len);
	}
	
	@Override
	public boolean ready() throws IOException {
		if(_reader==null){
			return false;
		}
		return _reader.ready();
	}
	
	@Override
	public void close() throws IOException {
		if(_reader==null){
			if(_src!=null){
				_src.close();
			}
		}
		_reader.close();
	}
	
	public String getCharset() throws IOException {
		initReader();
		return _charset;
	}
	
	private void initReader() throws IOException {
		if(_reader==null){
			if(_src==null){
				initSrc();
			}
			Pattern pattern = Pattern.compile("(?i)<meta\\s+" + // TODO HTML5への対応が必要
				"http-equiv=\"?Content-Type\"?\\s+content=\"[^\"]*?" +
				"charset\\s*=\\s*([^\"\\s]*)\\s*\"[^>]*>");
			do {
				int l = _src.read(_binBuf, _len, _binBuf.length - _len);
				if(l<0){
					InputStream ip1 = new ByteArrayInputStream(_binBuf, 0, _len);
					InputStream ip2 = new LinkedInputStream(ip1, _src);
					String charset = "UTF-8";
					_reader = new InputStreamReader(ip2, charset);
					_charset = charset;
					return;
				}
				for(int ii=0;ii<l;ii++){
					_headBuf.append((char)_binBuf[_len + ii]);
				}
				_len += l;
				Matcher matcher = pattern.matcher(_headBuf.toString());
				String charset = null;
				if(matcher.find()){
					String charset2 = matcher.group(1);
					charset = "UTF-8";
					if(charset2.equals("Shift_JIS")){
						charset = "MS932";
					} else if(charset2.equals("Shift-JIS")){
						charset = "MS932";
					} else if(charset2.equals("shift_jis")){
						charset = "MS932";
					} else if(charset2.equals("x-sjis")){
						charset = "MS932";
					} else if(charset2.equals("EUC-JP")){
						charset = "EUC_JP";
					} else if(charset2.equals("euc-jp")){
						charset = "EUC_JP";
					} else if(charset2.equals("iso-2022-jp")){
						charset = "ISO-2022-JP";
					} else if(charset2.equals("UTF-8")){
						charset = "UTF-8";
					} else if(charset2.equals("utf-8")){
						charset = "UTF-8";
					}
				} else if(_headBuf.length() >= _binBuf.length){
					charset = "UTF-8";
				}
				if(charset!=null){
					InputStream ip1 = new ByteArrayInputStream(_binBuf, 0, _len);
					InputStream ip2 = new LinkedInputStream(ip1, _src);
					_reader = new InputStreamReader(ip2, charset);
					_charset = charset;
					return;
				}
			} while(true);
		}
	}
	
	private void initSrc() throws IOException {
		URLConnection conn = (new URL(_url)).openConnection();
		// conn.setRequestProperty("User-Agent", "Mozilla");
		conn.connect();
		InputStream ip = conn.getInputStream();
		_src = ip;
	}
	
}
