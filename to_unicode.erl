-module(to_unicode).

-export([to_unicode/2]).
-export([encode_to_utf8/2]).

to_unicode(Input, Cs) when is_binary(Input) ->
	to_unicode_(binary_to_list(Input), Cs);
to_unicode(Input, Cs) when is_list(Input) ->
	to_unicode_(Input, Cs).

to_unicode_(Input, Cs) when Cs=='latin2';Cs=='iso-8859-2';Cs=='l2';
                           Cs=='iso_8859-2' ->
	latin2_to_unicode(Input).

latin2_to_unicode(Input) ->
	'to_unicode_table-8859-2':map(Input).


% Simpler API

% Enc0 is a string describing input encoding
% (it is case-insensitive, and support many encoding synonims)
encode_to_utf8(Enc0, Input) ->
	case find_encoding(Enc0) of
		{ok, Enc} ->
			Output = case Enc of
				utf8 ->
					Input;
				Other ->
					O2 = to_unicode(Input, Other),
					O3 = unicode:characters_to_list(O2, unicode),
					O4 = unicode:characters_to_binary(O3, unicode, unicode),
					O5 = unicode:characters_to_binary(O4, unicode, utf8),
					O5
			end,
			{ok, Output};
		unknown ->
			{error, unknown_encoding}
	end.


find_encoding(Enc0) ->
	case find_encoding0(string:to_lower(Enc0)) of
		unknown ->
			unknown;
		Enc ->
			{ok, Enc}
	end.


% Note: when adding new encoding, add it in lower-case!
%       In comment leave original correct caseing for documentation.


% Unicode - UTF-8
find_encoding0("utf-8") -> utf8;
find_encoding0("utf8") -> utf8;

% USA, UK, West and Central Europe - ISO 8859-1, ISO/IEC 8859-1
find_encoding0("iso-8859-1") -> 'iso-8859-1';
find_encoding0("iso_8859-1") -> 'iso-8859-1';
find_encoding0("iso 8859-1") -> 'iso-8859-1';
find_encoding0("iso8859-1") -> 'iso-8859-1';
find_encoding0("latin1") -> 'iso-8859-1';
find_encoding0("latin-1") -> 'iso-8859-1';
find_encoding0("iso/iec 8859-1") -> 'iso-8859-1';

% Central and East Europe - ISO 8859-2, ISO/IEC 8859-2
find_encoding0("iso-8859-2") -> 'iso-8859-2';
find_encoding0("iso_8859-2") -> 'iso-8859-2';
find_encoding0("iso 8859-2") -> 'iso-8859-2';
find_encoding0("iso8859-2") -> 'iso-8859-2';
find_encoding0("latin2") -> 'iso-8859-2';
find_encoding0("latin-2") -> 'iso-8859-2';
find_encoding0("iso/iec 8859-2") -> 'iso-8859-2';

% Japan -  Shift_JIS, "SHIFT-JIS", "shift-jis"
find_encoding0("shift-jis") -> 'shift-jis';
find_encoding0("shift_jis") -> 'shift-jis';

% cp932 % extension of Shift JIS
% "euc-jp" -> % "EUC-JP", "eucJP", "euc-jp"
%	{Data0, []};

% China, Taiwan, Korea
%	"gb2312" -> % GB 2312-1980, "GB2312-80", "GB"
%	{Data0, []};
% "gbk" -> % GBK is extension to GB2312
% "gb18030"  -> % newer GB2312
% "cp54936" % same as GB 18030?
% "cp396" % extended GB 2312 with most of GBK, by Microsoft
% "euc-cn" -> % "EUC-CN", mostly gb2312
% "big5" -> % Big5, "BIG5", "big5"
% "iso-2022-jp" -> % ISO 2022-JP
%           jp-1"
%           jp-2"
%           jp-3"
%           jp-2004"
%           kr"
%           cn"
%           cn-ext"
% "CNS 11643" % CNS 11643-1992
% ISO-IR-165
% KS X 1001-1992
% KS C 5601-1987, "KSC5601", "ksc5601", "KS_C_5601-1987"
% JIS X 0213-2000
% JIS X 0201-1976
% ISO/IEC 8859-7
% ISO/IEC 8859-1
% "euc-tw" -> % "EUC-TW", "euc-tw"
% "euc-kr" -> % "EUC-KR", "euc-kr"
% "ks-c-5861" -> % KS C 5861
% JIS C 6226-1978 aka 78JIS
% JIS C 6234-1983, printers
% JIS Z 8201-1981, math
% JIS Z 8202-1982, science
% JIS C 6226-1983 aka 83JIS
% JIS X 0208-1983
% JIS X 0208-1990 aka 90JIS
% JIS X 0208:1997 aka 97JIS
% JIS X 0202, ??
% JIS X 0213
% ISO/IEC 646 IRV
% JIS X 0201
% JIS X 0212
% JIS X 0221
% SJIS, same as Shift_JIS?
% cp932, generally SJIS
% JIPS
% KEIS
% MacJappanese
%
% KOI8-R % Russian and Bulgarian
% KOI8-U % Ukrainian and Belorussian
% KOI8-T % Tajik
% KOI8-CS % Czech, Slovakian
% KOI8-O % Old Russian
% KOR8-RU % ?
% KOI7 (with no small letters)
%
% TIS-620 similar to ISO/IEC 8859-11
% 
% cp437 aka ms-dos cp437
% ibm cp437
% cp852 aka ms-dos cp852
% ibm cp852, ibm pc 852
% windows-1250, cp1250, win1250
% windows-1252, microsoft-1252
% ibm pc 850
% ibm mvs 1047



% Unknown encodings
find_encoding0(_) -> unknown.

