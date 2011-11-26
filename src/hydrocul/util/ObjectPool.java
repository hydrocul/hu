package hydrocul.util;

import java.lang.ref.WeakReference;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class ObjectPool {
	
	private final ScheduledExecutorService _executor;
	private final HashMap<String, ObjectHolder> _map;
	private int _counter;
	
	public ObjectPool(ScheduledExecutorService executor){
		_executor = executor;
		_map = new HashMap<String, ObjectHolder>();
		_counter = 0;
	}
	
	/**
	 * オブジェクトを保存しそのキーを取得する。すでに保存されている場合は、
	 * そのままそのキーを取得する。
	 * @param obj 保存するオブジェクト
	 * @return キー
	 */
	public String getKey(Object obj){
		synchronized(_map){
			for(String key : _map.keySet()){
				ObjectHolder holder = _map.get(key);
				if(holder!=null){
					if(holder.hasObject(obj)){
						return key;
					}
				}
			}
			_counter++;
			if(_counter >= 20){
				_counter = 0;
				gc();
			}
			while(true){
				String key = createKey();
				if(_map.containsKey(key)){
					continue;
				}
				ObjectHolder holder = new ObjectHolder(obj);
				_map.put(key, holder);
				return key;
			}
			
		}
	}
	
	/**
	 * キーからオブジェクトを取得する。オブジェクトが見つからない場合は
	 * nullを返す。
	 * @param key キー
	 * @return 保存されていたオブジェクト
	 */
	public Object get(String key){
		startGc();
		ObjectHolder holder = _map.get(key);
		if(holder==null){
			return null;
		}
		return holder.get();
	}
	
	private volatile boolean _gc = false;
	
	private void startGc(){
		if(!_gc){
			_gc = true;
			_executor.schedule(new Runnable(){
				
				public void run(){
					gc();
					_gc = false;
				}
				
			}, 3, TimeUnit.SECONDS);
		}
	}
	
	private void gc(){
		synchronized(_map){
			HashSet<String> removeSet = new HashSet<String>();
			for(String key : _map.keySet()){
				ObjectHolder holder = _map.get(key);
				if(holder!=null){
					if(holder.canDelete()){
						removeSet.add(key);
					}
				}
			}
			for(String key : removeSet){
				_map.remove(key);
			}
		}
	}
	
	private static char[] _keyChars = {
//		'A','B','C','D','E','F','G','H','J','K','L','M','N','P','Q','R','S','T','U','V','W','X','Y','Z',
		'a','b','c','d','e','f','g','h','i','j','k','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
		'2','3','4','5','6','7','8','9'};

  protected static String createRandom(int len){
		StringBuilder ret = new StringBuilder();
		int n = _keyChars.length;
		for(int ii=0;ii<len;ii++){
			ret.append(_keyChars[(int)(Math.random() * n)]);
		}
		return ret.toString();
  }

  protected String createKey(){
    int size = _map.size();
		int length;
		if(size >= 10000){
			length = 4;
		} else {
			length = 3;
		}
    return createRandom(length);
	}
	
	private class ObjectHolder {
		
		private Object _obj;
		private final WeakReference<Object> _ref;
		private Date _date;
		
		public ObjectHolder(Object obj){
			_obj = obj;
			_ref = new WeakReference<Object>(_obj);
			_date = new Date();
		}
		
		public Object get(){
			Object obj = _obj;
			if(obj!=null){
				_date = new Date();
			} else {
				obj = _ref.get();
				if(obj!=null){
					_date = new Date();
					_obj = obj;
				}
			}
			return obj;
		}
		
		public boolean hasObject(Object target){
			Object obj = _obj;
			if(obj==null){
				obj = _ref.get();
				if(obj==null){
					return false;
				}
			}
			if(target==obj){
				return true;
			} else {
				return false;
			}
		}
		
		public boolean canDelete(){
			Object obj = _ref.get();
			if(obj==null){
				return true;
			}
			Date now = new Date();
			if( now.getTime() - _date.getTime() > 10 * 60 * 1000 ){
				_obj = null;
			}
			return false;
		}
		
	}
	
}
