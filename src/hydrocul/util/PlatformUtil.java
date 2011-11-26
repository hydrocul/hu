package hydrocul.util;

import java.io.File;

public class PlatformUtil {
	
	private PlatformUtil(){
		// nothing
	}
	
	public static boolean isWindows(){
		if(File.separatorChar=='\\'){
			return true;
		} else {
			return false;
		}
	}
	
	public static boolean isUNIX(){
		if(File.separatorChar=='/'){
			return true;
		} else {
			return false;
		}
	}
	
}
