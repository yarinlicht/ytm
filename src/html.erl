% YouTube Monitor -> html formatting
% Yarin Licht March 19 2013
-module(html).

-export([html_format_channel/3, html_format_list/3, htmlent_superlatin/1]).

% encode and return any characters in Str above the Latin-1 value as html entities
htmlent_superlatin (Str) ->
	Char = hd (Str),
	Sub = if Char <  128 -> [Char]
	       ; Char >= 128 -> "&#x" ++ integer_to_list (Char, 16) ++ ";"
	      end,
	if length (Str) =< 1 -> Sub
	 ; length (Str) >  1 -> Sub ++ htmlent_superlatin (tl (Str))
	end.

% return the possessive form of Str
possessive (Str) ->
	End = [lists:nth (length (Str), Str)],
	Str ++ case End of
		"s" -> "'"; "S" -> "'"; _ -> "'s"
	end.

% find and return the description of the Author channel from the list of Channels if there is one
find_author_desc (Author, Channels) ->
	{Name, _Interval, Description} = hd (Channels),
	Res = string:equal (Name, Author),
	if  Res =:= true                                    -> Description
	 ; (Res =:= false) andalso (length (Channels) =< 1) -> ""
	 ; (Res =:= false) andalso (length (Channels) >  1) -> find_author_desc (Author, tl (Channels))
	end.

% return whether or not Needle text can be found anywhere within Haystack, so long as the haystack items match the optional Filter
is_list_in_str (Needle, Haystack, Filter) ->
	if length (Haystack) =< 0 -> false
	 ; length (Haystack) >  0 -> is_list_in_str_nn (Needle, Haystack, Filter) 
	end.
is_list_in_str_nn (Needle, Haystack, Filter) ->
	{HayNeedle, HayFilter} = hd (Haystack),
	FilterRes = (length (HayFilter) =:= 0) orelse string:equal (Filter, HayFilter),
	Res = if FilterRes =:= true ->
		NeedleRes = string:str (Needle, HayNeedle),
		if NeedleRes >  0 -> true
		 ; NeedleRes =< 0 -> false
		end
	 ; FilterRes =:= false -> false
	end,
	if  Res =:= true                                    -> true
	 ; (Res =:= false) andalso (length (Haystack) =< 1) -> false
	 ; (Res =:= false) andalso (length (Haystack) >  1) -> is_list_in_str_nn (Needle, tl (Haystack), Filter)
	end.

% make and return the actual Vids list item html formatting,
% ShowAuthor, Config, and ListConfig, are just as they are in the wrapping function
html_format_vidlist (Vids, ShowAuthor, Config, ListConfig) ->
	{_ListName, _ListDesc, _ListInter, _ListChannels, ListHighlights} = ListConfig,
	{Id, Name, Author, _AId, Length, Date} = hd (Vids),
	CleanName = netw:htmlspecialchars (Name),
	CleanStdTime = netw:htmlspecialchars (Date),
	CleanNiceTime = time:to_pretty_time (time:from_isoex_time (Date)),
	if ShowAuthor =:= true  -> AuthorHtml = "<a href=\"../channels/" ++ netw:urlencode (Author) ++ ".html\">" ++ netw:htmlspecialchars (Author) ++ "</a>"
	 ; ShowAuthor =:= false -> AuthorHtml = ""
	end,
	CheckRes = is_list_in_str (string:to_lower (Name), ListHighlights, Author),
	if CheckRes =:= true  -> TrOpen = "<tr class=\"hilite\">"
	 ; CheckRes =:= false -> TrOpen = "<tr>"
	end,
	CNTCutPt = length (CleanNiceTime) - 9,
	Str = TrOpen ++ "<td><a href=\"http://www.youtube.com/watch?v=" ++ netw:urlencode (Id) ++ "\">" ++ CleanName ++ 
	      "<span><img src=\"http://i.ytimg.com/vi/" ++ netw:urlencode (Id) ++ "/default.jpg\" "
	      "alt=\"Cover of " ++ CleanName ++ "\" title=\"" ++ CleanName ++ "\" /></span></a></td>"
	      "<td>" ++ Length ++ "</td><td>" ++ AuthorHtml ++ "</td>"
	      "<td><time datetime=\"" ++ CleanStdTime ++ "\">" ++ string:substr (CleanNiceTime, 1, CNTCutPt)  ++ "</time></td>"
	      "<td><time datetime=\"" ++ CleanStdTime ++ "\">" ++ string:substr (CleanNiceTime, CNTCutPt + 1) ++ "</time></td></tr>\n",
	if length (Vids) =< 1 -> Str
	 ; length (Vids) >  1 -> Str ++ html_format_vidlist (tl (Vids), ShowAuthor, Config, ListConfig)
	end.

% make and return an html formatted list of Vids under the ListConfig as per the program Config,
% entitled as Name, which is visually shown with Img and links to Home.
% if ShowAuthor is true, the list is formatted as a compilation list, if it's false, it's formatted as a single channel's list
html_format_vidlist (Vids, ShowAuthor, Config, ListConfig, Name, Home, Img) ->
	{{DataPn, _CachePn, DefInter, _DefList, _LowestInter}, _ListConfigs} = Config,
	{_ListName, ListDesc, ListInter, ListChannels, _ListHighlights} = ListConfig,
	CleanName = netw:htmlspecialchars (Name),
	PCleanName = if ShowAuthor =:= false -> netw:htmlspecialchars (possessive (Name))
	              ; ShowAuthor =:= true  -> CleanName
	             end,
	AuthorDesc = if ShowAuthor =:= false -> find_author_desc (Name, ListChannels)
	              ; ShowAuthor =:= true  -> ListDesc
	             end,
	DescHtml = if length (AuthorDesc) >  0 -> "<br/><div>" ++ netw:htmlspecialchars (AuthorDesc) ++ "</div>"
	            ; length (AuthorDesc) =< 0 -> ""
	           end,
	NowTime  = calendar:universal_time (),
	LowInter = if ShowAuthor =:= false -> conf:get_specific_interval (ListChannels, DefInter, Name)
	            ; ShowAuthor =:= true  -> ListInter
	           end,
	ThenTime = calendar:gregorian_seconds_to_datetime (calendar:datetime_to_gregorian_seconds (NowTime) + LowInter),
	CurrentTime = "<time datetime=\"" ++ time:to_isoex_time (NowTime)  ++ "\">" ++ time:to_pretty_time (NowTime)  ++ "</time>",
	FutureTime  = "<time datetime=\"" ++ time:to_isoex_time (ThenTime) ++ "\">" ++ time:to_pretty_time (ThenTime) ++ "</time>",
	{FirstVidId, _, _, _, _, _} = hd (Vids),
	FirstVidFake = string:equal (FirstVidId, "<none>"),
	ContentHtml = if FirstVidFake =:= true  -> "<div class=\"msg\">" ++ CleanName ++ " doesn't have any videos.</div>"
	               ; FirstVidFake =:= false -> "<div class=\"list\"><table>\n" ++ html_format_vidlist (Vids, ShowAuthor, Config, ListConfig) ++ "</table></div>"
	              end,
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
	"<html xmlns=\"http://www.w3.org/1999/xhtml\">"
	"<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"
	"<title>" ++ PCleanName ++ " Recent Videos</title><link type=\"text/css\" rel=\"stylesheet\" href=\"" ++ DataPn ++ "/html.css\" /></head>"
	"<body><div class=\"space\"><div class=\"left\"></div><div class=\"center\">"
	"<header><div><span><span><a href=\"" ++ Home ++ "\">" ++ PCleanName ++ "</a></span> recent videos&nbsp;&nbsp;&nbsp;<a href=\"" ++ Home ++ "\">"
	"</span><img src=\"" ++ Img ++ "\" alt=\"Picture of " ++ CleanName ++ "\" /></a>" ++ DescHtml ++ "</div></header><main>" ++ ContentHtml ++ "</main>"
	"<footer><div><span>Generated " ++ CurrentTime ++ ", due for regeneration " ++ FutureTime ++ ".</span>"
	"<br/>Maintained by Chaiomanot's YouTube Monitor.</div></footer>"
	"</div><div class=\"right\"></div></div></body></html>".

% make and return an html formatted channel Vids list under the ListConfig as per the program Config
html_format_channel (Vids, Config, ListConfig) ->
	{_Id, _Name, Author, AId, _Length, _Date} = hd (Vids),
	html_format_channel (Vids, Author, AId, Config, ListConfig).
html_format_channel (Vids, Name, Id, Config, ListConfig) ->
	html_format_vidlist (Vids, false, Config, ListConfig, Name, "http://www.youtube.com/user/" ++ netw:urlencode (Name),
	                     "http://i1.ytimg.com/i/" ++ netw:urlencode (Id) ++ "/1.jpg").

% make and return an html formatted list of Vids under the ListConfig as per the program Config
html_format_list (Vids, Config, ListConfig) ->
	{{DataPn, _CachePn, _DefInter, _DefList, _LowestInter}, _ListConfigs} = Config,
	{ListName, _ListDesc, _ListInter, _ListChannels, _ListHighlights} = ListConfig,
	html_format_vidlist (Vids, true, Config, ListConfig, ListName,
	                     "https://www.youtube.com/my_subscriptions", DataPn ++ "/logo.jpg").

% eof
