% YouTube Monitor -> time parsing and formatting
% Yarin Licht March 17 2013
-module(time).

-export([from_isoex_time/1, to_pretty_time/1, to_isoex_time/1, to_pretty_duration/1, get_file_time/1, parse_interval/1]).

% decode and return Time from the ISO-8601 Extended format string
% to this program's native time format
from_isoex_time (Time) ->
	Year   = list_to_integer (string:substr (Time, 1,  4)),
	Month  = list_to_integer (string:substr (Time, 6,  2)),
	Day    = list_to_integer (string:substr (Time, 9,  2)),
	Hour   = list_to_integer (string:substr (Time, 12, 2)) rem 24,
	Minute = list_to_integer (string:substr (Time, 15, 2)),
	Second = list_to_integer (string:substr (Time, 18, 2)),
	{{Year, Month, Day}, {Hour, Minute, Second}}.

% encode and return Time from this program's native time format
% to a simple compact human readable time format string
to_pretty_time (Time) ->
	LocalTime = calendar:universal_time_to_local_time (Time),
	{{_Year, Month, Day}, {Hour, Minute, _Second}} = LocalTime,
	MonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],
	%MonthNames = ["January", "February", "March", "April", "May", "June",
	%              "July", "August", "September", "October", "November", "December"],
	if Hour =:= 0  -> Meridian = "AM", MerHour = 12
	 ; Hour <   12 -> Meridian = "AM", MerHour = Hour
	 ; Hour =:= 12 -> Meridian = "PM", MerHour = 12
	 ; Hour >   12 -> Meridian = "PM", MerHour = Hour - 12
	end,
	DayOrd = case Day rem 10 of
		1 -> "st"; 2 -> "nd"; 3 -> "rd"; _ -> "th"
	end,
	lists:nth (Month, MonthNames) ++ " " ++
	string:right (integer_to_list (Day),     2, hd (" ")) ++ DayOrd ++ " " ++
	string:right (integer_to_list (MerHour), 2, hd (" ")) ++ ":" ++
	string:right (integer_to_list (Minute),  2, hd ("0")) ++ " " ++ Meridian.

% encode and return Time from this program's native time format
% to the ISO-8601 Extended format string
to_isoex_time (Time) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = Time,
	string:right (integer_to_list (Year),   4, hd ("0")) ++ "-" ++
	string:right (integer_to_list (Month),  2, hd ("0")) ++ "-" ++
	string:right (integer_to_list (Day),    2, hd ("0")) ++ "T" ++
	string:right (integer_to_list (Hour),   2, hd ("0")) ++ ":" ++
	string:right (integer_to_list (Minute), 2, hd ("0")) ++ ":" ++
	string:right (integer_to_list (Second), 2, hd ("0")) ++ ".000Z".

% encode and return Seconds to a simple compact human readable time format string
to_pretty_duration (Seconds) ->
	LowSec = Seconds rem 60,
	Minutes = (Seconds div 60),
	LowMin = Minutes rem 60,
	Hours = (Minutes div 60),
	if Hours >  0 -> HighStr = integer_to_list (Hours) ++ ":" ++ string:right (integer_to_list (LowMin), 2, hd ("0"))
	 ; Hours =< 0 -> HighStr = integer_to_list (LowMin)
	end,
	HighStr ++ ":" ++ string:right (integer_to_list (LowSec), 2, hd ("0")).

% get file Pathname last modified time
get_file_time (Pathname) ->
	case file:read_file_info (Pathname, [{time, universal}]) of
		{ok, {file_info, _, _, _, _ATime, MTime, _CTime, _, _, _, _, _, _, _}} -> MTime;
		{error, _Reason} -> {{1970, 1, 1}, {0, 0, 0}}
	end.

% parse and return the interval Str to seconds
parse_interval (Str) ->
	try
		[NumDigit, MultCode] = Str,
		Mult = case [MultCode] of
			"h" -> 3600; "d" -> 86400; "w" -> 604800; "m" -> 2629739
		end,
		list_to_integer ([NumDigit]) * Mult
	catch
		error:Desc -> error ({"parsing 'interval'", Str, Desc})
	end.

% eof
