package hydrocul.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class LinkedInputStream extends InputStream {
	
	private List<InputStream> _readerList;
	private int _index;
	private InputStream _curr;
	
	public LinkedInputStream(InputStream reader1, InputStream reader2){
		_readerList = new ArrayList<InputStream>();
		_readerList.add(reader1);
		_readerList.add(reader2);
		_index = 0;
		if(_readerList.size()==0){
			_curr = null;
		} else {
			_curr = _readerList.get(_index);
		}
	}
	
	public LinkedInputStream(List<InputStream> readerList){
		_readerList = readerList;
		_index = 0;
		if(_readerList.size()==0){
			_curr = null;
		} else {
			_curr = _readerList.get(_index);
		}
	}
	
	@Override
	public int read() throws IOException {
		if(_curr==null)
			return -1;
		int ret;
		while(true){
			ret = _curr.read();
			if(ret>=0)
				break;
			_index++;
			if(_index==_readerList.size()){
				_curr = null;
				return -1;
			}
			_curr = _readerList.get(_index);
		}
		return ret;
	}
	
	@Override
	public int read(byte[] buf, int off, int len) throws IOException {
		if(_curr==null)
			return -1;
		int ret;
		while(true){
			ret = _curr.read(buf, off, len);
			if(ret>=0)
				break;
			_index++;
			if(_index==_readerList.size()){
				_curr = null;
				return -1;
			}
			_curr = _readerList.get(_index);
		}
		return ret;
	}
	
	@Override
	public void close() throws IOException {
		IOException e = null;
		int n = _readerList.size();
		for(int ii=0;ii<n;ii++){
			try {
				_readerList.get(ii).close();
			} catch(IOException e2){
				if(e==null)
					e = e2;
			}
		}
		if(e!=null)
			throw e;
	}
	
}
