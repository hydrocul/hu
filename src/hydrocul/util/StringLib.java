package hydrocul.util;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringLib {
	
	private StringLib(){ /* nothing */ }
	
	private static final char[] _charTable;
	private static final int[] _hexTable;
	
	static {
		_charTable = new char[]{
			'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
			'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
			'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
			'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
			'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
		};
		_hexTable = new int[256];
		for(int ii=0;ii<256;ii++){
			_hexTable[ii] = -1;
		}
		for(int ii=0;ii<10;ii++){
			_hexTable['0' + ii] = ii;
		}
		for(int ii=0;ii<6;ii++){
			_hexTable['A' + ii] = 10 + ii;
		}
		for(int ii=0;ii<6;ii++){
			_hexTable['a' + ii] = 10 + ii;
		}
	}
	
	/**
	 * 任意のデータをSQLで使えるようにするためにエンコードする。<br>
	 */
	public static String encodeSql(String str){
		if(str==null)
			throw new NullPointerException();
		str = str.replaceAll("\\\\", "\\\\\\\\");
		str = str.replaceAll("\\r", "\\\\r");
		str = str.replaceAll("\\n", "\\\\n");
		str = str.replaceAll("'", "\\\\'");
		str = str.replaceAll("\"", "\\\\\"");
		return str;
	}
	
	/**
	 * 任意のデータをSQLのあいまい検索で使えるようにするためにエンコードする。<br>
	 */
	public static String encodeSqlLike(String str){
		str = encodeSql(str);
		str = str.replaceAll("%", "\\\\%");
		str = str.replaceAll("\\*", "%");
		str = str.replaceAll("_", "\\\\_");
		str = str.replaceAll("\\?", "_");
		return str;
	}
	
	/**
	 * 任意のデータをHTMLで表示させるためにエンコードする。改行文字を含むデータは
	 * 改行が無視される。<br>
	 */
	public static String encodeHtml(String str){
		if(str==null)
			throw new NullPointerException();
		str = str.replaceAll("&", "&amp;");
		str = str.replaceAll("\"", "&quot;");
		str = str.replaceAll("<", "&lt;");
		str = str.replaceAll(">", "&gt;");
		return str;
	}
	
	/**
	 * 任意のデータをHTMLで表示させるためにエンコードする。改行文字とURLを正しく処理
	 * する。<br>
	 */
	public static String encodeHtmlLong(String str){
		str = encodeHtml(str);
		str = str.replaceAll("\\r\\n", "\n");
		str = str.replaceAll("\\r", "\n");
		str = str.replaceAll("\\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
		str = str.replaceAll("\\n", "<br />\n");
		str = str.replaceAll("(https?://[-.%+/=?_~#&a-zA-Z0-9]*)",
				"<a href=\"$1\" target=\"_blank\">$1</a>");
		str = str.replaceAll("([-._a-zA-Z0-9]+@[-._a-zA-Z0-9]+)",
				"<a href=\"mailto:$1\">$1</a>");
		str = str.replaceAll("([ \\r\\n\\t]|&nbsp;) ([ \\r\\n\\t])", "$1&nbsp;$2");
		return str;
	}
	
	/**
	 * 任意のデータをHTMLで表示させるためにエンコードする。改行文字とURLを正しく処理
	 * する。HTMLタグはそのまま。<br>
	 */
	public static String encodeHtmlWeak(String str){
		
		str = str.replaceAll("&", "&amp;");
		str = str.replaceAll("\\r\\n", "\n");
		str = str.replaceAll("\\r", "\n");
		str = str.replaceAll("&amp;lt;", "&lt;");
		str = str.replaceAll("&amp;gt;", "&gt;");
		
		{
			Pattern pattern = Pattern.compile("(<pre( [^>]*)?>)"
					+ "(([^<]|<[^/]|</[^p]|</p[^r]|</pr[^e]|</pre[^/])*)\n"
					+ "(([^<]|<[^/]|</[^p]|</p[^r]|</pr[^e]|</pre[^/])*)"
					+ "(</pre>)", Pattern.CASE_INSENSITIVE);
			while(true){
				Matcher matcher = pattern.matcher(str);
				String str2 = matcher.replaceAll("$1$3\r$5$7");
				if(str2.equals(str)){
					break;
				}
				str = str2;
			}
		}
		
		{
			Pattern pattern = Pattern.compile("("
					+ "<br( [^/>]*/)?>"
					+ "|<li( [^/>]*)?>"
					+ "|</li>"
					+ "|<ul( [^/>]*)?>"
					+ "|</ul>"
					+ "|<ol( [^/>]*)?>"
					+ "|</ol>"
					+ ")\\n", Pattern.CASE_INSENSITIVE);
			Matcher matcher = pattern.matcher(str);
			str = matcher.replaceAll("$1\r");
		}
		
		str = str.replaceAll(" ", "&nbsp;");
		str = str.replaceAll("\\n", "<br />\n");
		str = str.replaceAll("\\r", "\n");
		str = str.replaceAll("\\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
		str = str.replaceAll("([a-zA-Z.,;:?!])&nbsp;([a-zA-Z.,;:?!])", "$1 $2");
		
		{
			Pattern pattern = Pattern.compile("(^|[^>\"'])(https?://[-.%+/=?_~#a-zA-Z0-9]*+)", Pattern.DOTALL);
			Matcher matcher = pattern.matcher(str);
			str = matcher.replaceAll("$1<a href=\"$2\" target=_blank>$2</a>");
		}
		{
			Pattern pattern = Pattern.compile("([-._a-zA-Z0-9]+@[-._a-zA-Z0-9]++)", Pattern.DOTALL);
			Matcher matcher = pattern.matcher(str);
			str = matcher.replaceAll("<a href=\"mailto:$1\">$1</a>");
		}
		
		return str;
		
	}
	
	/**
	 * 任意のデータをHTMLコメントの中で表示させるためにエンコードする。<br>
	 */
	public static String encodeHtmlComment(String str){
		if(str==null)
			throw new NullPointerException();
		str = str.replaceAll("--", "&#x2D");
		return str;
	}

	/**
	 * 任意のデータをURLの引数にするためにエンコードする。<br>
	 */
	public static String encodeUrl(String str){
		if(str==null)
			throw new NullPointerException();
		try {
			return URLEncoder.encode(str, "UTF-8");
		} catch(UnsupportedEncodingException e){
			Error e2 = new AssertionError();
			e2.initCause(e);
			throw e2;
		}
	}
	
	public static String decodeUrl(String str){
		if(str==null)
			throw new NullPointerException();
		try {
			return URLDecoder.decode(str, "UTF-8");
		} catch(UnsupportedEncodingException e){
			Error e2 = new AssertionError();
			e2.initCause(e);
			throw e2;
		}
	}
	
	public static String encodeJavaLiteral(String str){
		if(str==null)
			throw new NullPointerException();
		int index = 0;
		int len = str.length();
		StringBuilder sb = new StringBuilder();
		for(;;){
			if(index==len)
				break;
			char ch = str.charAt(index);
			if(ch=='\b'){
				sb.append('\\');
				sb.append('b');
			} else if(ch=='\t'){
				sb.append('\\');
				sb.append('t');
			} else if(ch=='\n'){
				sb.append('\\');
				sb.append('n');
			} else if(ch=='\f'){
				sb.append('\\');
				sb.append('f');
			} else if(ch=='\r'){
				sb.append('\\');
				sb.append('r');
			} else if(ch=='\"'){
				sb.append('\\');
				sb.append('\"');
			} else if(ch=='\''){
				sb.append('\\');
				sb.append('\'');
			} else if(ch=='\\'){
				sb.append('\\');
				sb.append('\\');
			} else if(ch < 0x20){
				sb.append('\\');
				sb.append('x');
				sb.append(Integer.toString(ch, 16));
			} else {
				sb.append(ch);
			}
			index++;
		}
		return sb.toString();
	}
	
	public static String decodeJavaLiteral(String str){
		if(str==null)
			throw new NullPointerException();
		int index = 0;
		int len = str.length();
		StringBuilder sb = new StringBuilder();
		boolean yen = false;
		for(;;){
			if(index==len){
				if(yen){
					sb.append('\\');
				}
				break;
			}
			char ch = str.charAt(index);
			if(yen){
				if(ch=='b'){
					sb.append('\b');
				} else if(ch=='t'){
					sb.append('\t');
				} else if(ch=='n'){
					sb.append('\n');
				} else if(ch=='f'){
					sb.append('\f');
				} else if(ch=='r'){
					sb.append('\r');
				} else if(ch=='\"'){
					sb.append('\"');
				} else if(ch=='\''){
					sb.append('\'');
				} else if(ch=='\\'){
					sb.append('\\');
				} else {
					sb.append('\\');
					sb.append(ch);
				}
				yen = false;
			} else if(ch=='\\'){
				yen = true;
			} else {
				sb.append(ch);
			}
			index++;
		}
		return sb.toString();
	}
	
	/**
	 * 指定の長さのランダムな文字列を生成する。文字列を構成する文字は、
	 * <code>0-9a-zA-Z</code>である。lenに負を与えた場合は、
	 * {@link IllegalArgumentException} をスローする。<br>
	 */
	public static String createRandomString(int length){
		if(length<0)
			throw new IllegalArgumentException();
		StringBuffer ret = new StringBuffer();
		for(int ii=0;ii<length;ii++){
			ret.append(_charTable[(int)(Math.random() * _charTable.length)]);
		}
		return ret.toString();
	}
	
	/**
	 * 1バイトの整数を2桁の16進数表記に書式化する。引数の最下位の1バイトのみを
	 * 見る。<br>
	 */
	public static String byteToHex2(int b){
		StringBuffer ret = new StringBuffer();
		{
			int c = (b & 0xf0) >>> 4;
			char d;
			if(c>=10){
				d = (char)('A' + c - 10);
			} else {
				d = (char)('0' + c);
			}
			ret.append(d);
		}
		{
			int c = b & 0x0f;
			char d;
			if(c>=10){
				d = (char)('A' + c - 10);
			} else {
				d = (char)('0' + c);
			}
			ret.append(d);
		}
		return ret.toString();
	}
	
	/**
	 * 2バイトの整数を4桁の16進数表記に書式化する。引数の最下位の2バイトのみを
	 * 見る。<br>
	 */
	public static String byteToHex4(int b){
		StringBuffer ret = new StringBuffer();
		{
			int c = (b & 0xf000) >>> 12;
			char d;
			if(c>=10){
				d = (char)('A' + c - 10);
			} else {
				d = (char)('0' + c);
			}
			ret.append(d);
		}
		{
			int c = (b & 0x0f00) >>> 8;
			char d;
			if(c>=10){
				d = (char)('A' + c - 10);
			} else {
				d = (char)('0' + c);
			}
			ret.append(d);
		}
		{
			int c = (b & 0x00f0) >>> 4;
			char d;
			if(c>=10){
				d = (char)('A' + c - 10);
			} else {
				d = (char)('0' + c);
			}
			ret.append(d);
		}
		{
			int c = b & 0x000f;
			char d;
			if(c>=10){
				d = (char)('A' + c - 10);
			} else {
				d = (char)('0' + c);
			}
			ret.append(d);
		}
		return ret.toString();
	}
	
	/**
	 * 2桁の16進数表記を1バイトの整数に変換する。正の数を返す。16進数表記でない
	 * 場合は {@link IllegalArgumentException} をスローする。<br>
	 */
	public static int hexToByte2(char[] h, int off){
		int p0 = _hexTable[h[off+0]];
		if(p0<0)
			throw new NumberFormatException(new String(h, off, 2));
		int p1 = _hexTable[h[off+1]];
		if(p1<0)
			throw new NumberFormatException(new String(h, off, 2));
		return p0 * 16 + p1;
	}
	
	/**
	 * 4桁の16進数表記を2バイトの整数に変換する。正の数を返す。16進数表記でない
	 * 場合は {@link IllegalArgumentException} をスローする。<br>
	 */
	public static int hexToByte4(char[] h, int off){
		int p0 = _hexTable[h[off+0]];
		if(p0<0)
			throw new NumberFormatException(new String(h, off, 4));
		int p1 = _hexTable[h[off+1]];
		if(p1<0)
			throw new NumberFormatException(new String(h, off, 4));
		int p2 = _hexTable[h[off+2]];
		if(p2<0)
			throw new NumberFormatException(new String(h, off, 4));
		int p3 = _hexTable[h[off+3]];
		if(p3<0)
			throw new NumberFormatException(new String(h, off, 4));
		return p0 * 4096 + p1 * 256 + p2 * 16 + p3;
	}
	
	public static int lengthOnTerminal(String str){
    String str2 = new scala.collection.immutable.StringOps("\u001B[^m]*m").r().replaceAllIn(str, "");
    try {
      return str2.getBytes("SJIS").length;
    } catch(UnsupportedEncodingException e){
      throw new AssertionError(e);
    }
	}
	
	public static String[] parseArgs(String args){
		
		ArrayList<String> ret = new ArrayList<String>();
		
		int len = args.length();
		boolean quoted = false;
		boolean prevQuote = false;
		StringBuilder buf = new StringBuilder();
		int index = 0;
		while(true){
			if(index==len){
				if(buf.length() > 0){
					ret.add(buf.toString());
				}
				break;
			}
			char ch = args.charAt(index);
			index++;
			if(quoted){
				if(prevQuote){
					if(ch=='\"'){
						buf.append(ch);
					} else if(ch==' ' || ch=='\t'){
						ret.add(buf.toString());
						buf = new StringBuilder();
						quoted = false;
					} else {
						buf.append('\"');
						buf.append(ch);
					}
					prevQuote = false;
				} else {
					if(ch=='\"'){
						prevQuote = true;
					} else {
						buf.append(ch);
					}
				}
			} else {
				if(ch=='\"' && buf.length()==0){
					quoted = true;
				} else if(ch==' ' || ch=='\t'){
					if(buf.length() > 0){
						ret.add(buf.toString());
						buf = new StringBuilder();
					}
				} else {
					buf.append(ch);
				}
			}
		}
		
		String[] ret2 = new String[ret.size()];
		for(int ii=0;ii<ret2.length;ii++){
			ret2[ii] = ret.get(ii);
		}
		return ret2;
		
	}
	
}
