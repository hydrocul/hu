package hydrocul.util;

public class RomaKanaHenkan {
	
	private static final String[][] _convertionTable1;
	private static final String[] _convertionTable2;
	
	static {
		_convertionTable1 = new String[][]{
				{"a",	"あ"},
				{"i",	"い"},
				{"u",	"う"},
				{"e",	"え"},
				{"o",	"お"},
				{"ka",	"か"},
				{"ki",	"き"},
				{"ku",	"く"},
				{"ke",	"け"},
				{"ko",	"こ"},
				{"sa",	"さ"},
				{"si",	"し"},
				{"su",	"す"},
				{"se",	"せ"},
				{"so",	"そ"},
				{"ta",	"た"},
				{"ti",	"ち"},
				{"tu",	"つ"},
				{"te",	"て"},
				{"to",	"と"},
				{"na",	"な"},
				{"ni",	"に"},
				{"nu",	"ぬ"},
				{"ne",	"ね"},
				{"no",	"の"},
				{"ha",	"は"},
				{"hi",	"ひ"},
				{"hu",	"ふ"},
				{"he",	"へ"},
				{"ho",	"ほ"},
				{"ma",	"ま"},
				{"mi",	"み"},
				{"mu",	"む"},
				{"me",	"め"},
				{"mo",	"も"},
				{"ya",	"や"},
				{"yi",	"い"},
				{"yu",	"ゆ"},
				{"ye",	"いぇ"},
				{"yo",	"よ"},
				{"ra",	"ら"},
				{"ri",	"り"},
				{"ru",	"る"},
				{"re",	"れ"},
				{"ro",	"ろ"},
				{"wa",	"わ"},
				{"wi",	"うぃ"},
				{"wu",	"う"},
				{"we",	"うぇ"},
				{"wo",	"を"},
				{"ga",	"が"},
				{"gi",	"ぎ"},
				{"gu",	"ぐ"},
				{"ge",	"げ"},
				{"go",	"ご"},
				{"za",	"ざ"},
				{"zi",	"じ"},
				{"zu",	"ず"},
				{"ze",	"ぜ"},
				{"zo",	"ぞ"},
				{"da",	"だ"},
				{"di",	"ぢ"},
				{"du",	"づ"},
				{"de",	"で"},
				{"do",	"ど"},
				{"ba",	"ば"},
				{"bi",	"び"},
				{"bu",	"ぶ"},
				{"be",	"べ"},
				{"bo",	"ぼ"},
				{"pa",	"ぱ"},
				{"pi",	"ぴ"},
				{"pu",	"ぷ"},
				{"pe",	"ぺ"},
				{"po",	"ぽ"},
				{"va",	"ヴぁ"},
				{"vi",	"ヴぃ"},
				{"vu",	"ヴ"},
				{"ve",	"ヴぇ"},
				{"vo",	"ヴぉ"},
				{"ca",	"か"},
				{"ci",	"し"},
				{"cu",	"く"},
				{"ce",	"せ"},
				{"co",	"こ"},
				{"qa",	"くぁ"},
				{"qi",	"くぃ"},
				{"qu",	"く"},
				{"qe",	"くぇ"},
				{"qo",	"くぉ"},
				{"fa",	"ふぁ"},
				{"fi",	"ふぃ"},
				{"fu",	"ふ"},
				{"fe",	"ふぇ"},
				{"fo",	"ふぉ"},
				{"ja",	"じゃ"},
				{"ji",	"じ"},
				{"ju",	"じゅ"},
				{"je",	"じぇ"},
				{"jo",	"じょ"},
				{"kya",	"きゃ"},
				{"kyi",	"きぃ"},
				{"kyu",	"きゅ"},
				{"kye",	"きぇ"},
				{"kyo",	"きょ"},
				{"qya",	"くゃ"},
				{"qyi",	"くぃ"},
				{"qyu",	"くゅ"},
				{"qye",	"くぇ"},
				{"qyo",	"くょ"},
				{"sya",	"しゃ"},
				{"syi",	"しぃ"},
				{"syu",	"しゅ"},
				{"sye",	"しぇ"},
				{"syo",	"しょ"},
				{"tya",	"ちゃ"},
				{"tyi",	"ちぃ"},
				{"tyu",	"ちゅ"},
				{"tye",	"ちぇ"},
				{"tyo",	"ちょ"},
				{"cya",	"ちゃ"},
				{"cyi",	"ちぃ"},
				{"cyu",	"ちゅ"},
				{"cye",	"ちぇ"},
				{"cyo",	"ちょ"},
				{"nya",	"にゃ"},
				{"nyi",	"にぃ"},
				{"nyu",	"にゅ"},
				{"nye",	"にぇ"},
				{"nyo",	"にょ"},
				{"hya",	"ひゃ"},
				{"hyi",	"ひぃ"},
				{"hyu",	"ひゅ"},
				{"hye",	"ひぇ"},
				{"hyo",	"ひょ"},
				{"fya",	"ふゃ"},
				{"fyi",	"ふぃ"},
				{"fyu",	"ふゅ"},
				{"fye",	"ふぇ"},
				{"fyo",	"ふょ"},
				{"mya",	"みゃ"},
				{"myi",	"みぃ"},
				{"myu",	"みゅ"},
				{"mye",	"みぇ"},
				{"myo",	"みょ"},
				{"rya",	"りゃ"},
				{"ryi",	"りぃ"},
				{"ryu",	"りゅ"},
				{"rye",	"りぇ"},
				{"ryo",	"りょ"},
				{"vya",	"ヴゃ"},
				{"vyi",	"ヴぃ"},
				{"vyu",	"ヴゅ"},
				{"vye",	"ヴぇ"},
				{"vyo",	"ヴょ"},
				{"gya",	"ぎゃ"},
				{"gyi",	"ぎぃ"},
				{"gyu",	"ぎゅ"},
				{"gye",	"ぎぇ"},
				{"gyo",	"ぎょ"},
				{"zya",	"じゃ"},
				{"zyi",	"じぃ"},
				{"zyu",	"じゅ"},
				{"zye",	"じぇ"},
				{"zyo",	"じょ"},
				{"jya",	"じゃ"},
				{"jyi",	"じぃ"},
				{"jyu",	"じゅ"},
				{"jye",	"じぇ"},
				{"jyo",	"じょ"},
				{"dya",	"ぢゃ"},
				{"dyi",	"ぢぃ"},
				{"dyu",	"ぢゅ"},
				{"dye",	"ぢぇ"},
				{"dyo",	"ぢょ"},
				{"bya",	"びゃ"},
				{"byi",	"びぃ"},
				{"byu",	"びゅ"},
				{"bye",	"びぇ"},
				{"byo",	"びょ"},
				{"pya",	"ぴゃ"},
				{"pyi",	"ぴぃ"},
				{"pyu",	"ぴゅ"},
				{"pye",	"ぴぇ"},
				{"pyo",	"ぴょ"},
				{"sha",	"しゃ"},
				{"shi",	"し"},
				{"shu",	"しゅ"},
				{"she",	"しぇ"},
				{"sho",	"しょ"},
				{"cha",	"ちゃ"},
				{"chi",	"ち"},
				{"chu",	"ちゅ"},
				{"che",	"ちぇ"},
				{"cho",	"ちょ"},
				{"tha",	"てゃ"},
				{"thi",	"てぃ"},
				{"thu",	"てゅ"},
				{"the",	"てぇ"},
				{"tho",	"てょ"},
				{"dha",	"でゃ"},
				{"dhi",	"でぃ"},
				{"dhu",	"でゅ"},
				{"dhe",	"でぇ"},
				{"dho",	"でょ"},
				{"wha",	"うぁ"},
				{"whi",	"うぃ"},
				{"whu",	"う"},
				{"whe",	"うぇ"},
				{"who",	"うぉ"},
				{"kwa",	"くぁ"},
				{"kwi",	"くぃ"},
				{"kwu",	"くぅ"},
				{"kwe",	"くぇ"},
				{"kwo",	"くぉ"},
				{"qwa",	"くぁ"},
				{"qwi",	"くぃ"},
				{"qwu",	"くぅ"},
				{"qwe",	"くぇ"},
				{"qwo",	"くぉ"},
				{"gwa",	"ぐぁ"},
				{"gwi",	"ぐぃ"},
				{"gwu",	"ぐぅ"},
				{"gwe",	"ぐぇ"},
				{"gwo",	"ぐぉ"},
				{"swa",	"すぁ"},
				{"swi",	"すぃ"},
				{"swu",	"すぅ"},
				{"swe",	"すぇ"},
				{"swo",	"すぉ"},
				{"twa",	"とぁ"},
				{"twi",	"とぃ"},
				{"twu",	"とぅ"},
				{"twe",	"とぇ"},
				{"two",	"とぉ"},
				{"dwa",	"どぁ"},
				{"dwi",	"どぃ"},
				{"dwu",	"どぅ"},
				{"dwe",	"どぇ"},
				{"dwo",	"どぉ"},
				{"fwa",	"ふぁ"},
				{"fwi",	"ふぃ"},
				{"fwu",	"ふぅ"},
				{"fwe",	"ふぇ"},
				{"fwo",	"ふぉ"},
				{"tsa",	"つぁ"},
				{"tsi",	"つぃ"},
				{"tsu",	"つ"},
				{"tse",	"つぇ"},
				{"tso",	"つぉ"},
				{"la",	"ぁ"},
				{"li",	"ぃ"},
				{"lu",	"ぅ"},
				{"le",	"ぇ"},
				{"lo",	"ぉ"},
				{"xa",	"ぁ"},
				{"xi",	"ぃ"},
				{"xu",	"ぅ"},
				{"xe",	"ぇ"},
				{"xo",	"ぉ"},
				{"lka",	"ヵ"},
				{"xka",	"ヵ"},
				{"lke",	"ヶ"},
				{"xke",	"ヶ"},
				{"ltu",	"っ"},
				{"ltsu",	"っ"},
				{"xtu",	"っ"},
				{"lya",	"ゃ"},
				{"lyi",	"ぃ"},
				{"lyu",	"ゅ"},
				{"lye",	"ぇ"},
				{"lyo",	"ょ"},
				{"xya",	"ゃ"},
				{"xyi",	"ぃ"},
				{"xyu",	"ゅ"},
				{"xye",	"ぇ"},
				{"xyo",	"ょ"},
				{"lwa",	"ゎ"},
				{"xwa",	"ゎ"},
				{"nn",	"ん"},
				{"n'",	"ん"},
				{"xn",	"ん"},
				{"-",	"ー"},
		};
		_convertionTable2 = new String[]{
				"kk",
				"ss",
				"tt",
				"tc",
				"hh",
				"mm",
				"yy",
				"rr",
				"ww",
				"gg",
				"zz",
				"dd",
				"bb",
				"pp",
				"vv",
				"cc",
				"qq",
				"ff",
				"jj",
				"ll",
				"xx",
		};
	}
	
	private RomaKanaHenkan(){ /* nothing */ }
	
	public static String roma2kana(String roma){
		int index = 0;
		StringBuilder ret = new StringBuilder();
		int len = roma.length();
		loop1: while(true){
			if(index >= len){
				return ret.toString();
			}
			for(int ii=0;ii<_convertionTable1.length;ii++){
				String s = _convertionTable1[ii][0];
				if(roma.startsWith(s, index)){
					ret.append(_convertionTable1[ii][1]);
					index += s.length();
					continue loop1;
				}
			}
			for(int ii=0;ii<_convertionTable2.length;ii++){
				String s = _convertionTable2[ii];
				if(roma.startsWith(s, index)){
					ret.append("っ");
					index++;
					continue loop1;
				}
			}
			if(roma.startsWith("n'", index)){
				ret.append("ん");
				index += 2;
				continue loop1;
			}
			if(roma.startsWith("n", index) && index < len - 1){
				ret.append("ん");
				index++;
				continue loop1;
			}
			ret.append(roma.charAt(index));
			index++;
		}
	}
	
	public static String hiragana2katakana(String hiragana){
		StringBuilder ret = new StringBuilder();
		int len = hiragana.length();
		int index = 0;
		while(true){
			if(index >= len){
				break;
			}
			char ch = hiragana.charAt(index);
			if(ch >= 'ぁ' && ch <= 'ん'){
				ret.append((char)(ch - 'ぁ' + 'ァ'));
			} else {
				ret.append(ch);
			}
			index++;
		}
		return ret.toString();
	}
	
}
