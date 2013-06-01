% YouTube Monitor -> configuration
% Yarin Licht March 19 2013
-module(conf).

-export([get_config/1, get_specific_interval/3, get_broad_specific_interval/3]).

% find and return the interval of the Author channel from all the lists of channels in the ListConfig
% if the specific channel can't be found, returns the Default interval instead
get_broad_specific_interval (ListConfig, Default, Author) ->
	{_Name, _Desc, _Inter, Channels, _Highlights} = hd (ListConfig),
	Res = get_specific_interval (Channels, searching, Author),
	if  Res =/= searching                                     -> Res
	 ; (Res =:= searching) andalso (length (ListConfig) =< 1) -> Default
	 ; (Res =:= searching) andalso (length (ListConfig) >  1) -> get_broad_specific_interval (tl (ListConfig), Default, Author)
	end.

% find and return the interval of the Author channel from the list of Channels
% if the specific channel can't be found, returns the Default interval instead
get_specific_interval (Channels, Default, Author) ->
	{Name, Interval, _Description} = hd (Channels),
	Res = string:equal (Name, Author),
	if  Res =:= true                                    -> Interval
	 ; (Res =:= false) andalso (length (Channels) =< 1) -> Default
	 ; (Res =:= false) andalso (length (Channels) >  1) -> get_specific_interval (tl (Channels), Default, Author)
	end.

% find and return the lowest interval from among the list of Channels and the explicit interval InterB
get_lowest_interval (Channels, InterB) ->
	{_Name, Interval, _Description} = hd (Channels),
	Max = if Interval <  InterB -> Interval
	       ; Interval >= InterB -> InterB
	      end,
	if length (Channels) =< 1 -> Max
	 ; length (Channels) >  1 -> get_lowest_interval (tl (Channels), Max)
	end.

% find and return the lowest interval from among the Lists and the explicit interval InterB
get_lowest_global_interval (Lists, InterB) ->
	{_Name, _Desc, Interval, _Channels, _Highlights} = hd (Lists),
	Max = if Interval <  InterB -> Interval
	       ; Interval >= InterB -> InterB
	      end,
	if length (Lists) =< 1 -> Max
	 ; length (Lists) >  1 -> get_lowest_global_interval (tl (Lists), Max)
	end.

% parse and return the Entry highlight of ListXPath list of the configuration XmlRoot
parse_highlight (XmlRoot, ListXPath, Entry) ->
	try
		ChanXPath = ListXPath ++ "/highlight[" ++ integer_to_list (Entry + 1) ++ "]",
		StringAttr  = xmerl_xpath:string (ChanXPath ++ "/@string",  XmlRoot),
		ChannelAttr = xmerl_xpath:string (ChanXPath ++ "/@channel", XmlRoot),
		if length (StringAttr) >  0 -> {xmlAttribute, string, _, _, _, _, _, _, String, _} = hd (StringAttr)
		 ; length (StringAttr) =< 0 -> String = "", exit ("'channel' is missing required 'string'")
		end,
		if length (ChannelAttr) >  0 -> {xmlAttribute, channel, _, _, _, _, _, _, Channel, _} = hd (ChannelAttr)
		 ; length (ChannelAttr) =< 0 -> Channel = ""
		end,
		{string:to_lower (String), Channel}
	catch
		exit:Desc -> error ({"parsing 'highlight'", Entry, Desc})
	end.

% parse and return Entry through LastEntry of the ListXPath list's highlights of the configuration XmlRoot
parse_highlights (XmlRoot, ListXPath, Entry, LastEntry) ->
	if Entry >= LastEntry -> []; Entry < LastEntry ->
	[parse_highlight (XmlRoot, ListXPath, Entry)] ++ parse_highlights (XmlRoot, ListXPath, Entry + 1, LastEntry)
	end.

% parse and return the Entry channel of ListXPath list of the configuration XmlRoot
parse_channel (XmlRoot, ListXPath, Entry) ->
	try
		ChanXPath = ListXPath ++ "/channel[" ++ integer_to_list (Entry + 1) ++ "]",
		NameAttr        = xmerl_xpath:string (ChanXPath ++ "/@name",        XmlRoot),
		IntervalAttr    = xmerl_xpath:string (ChanXPath ++ "/@interval",    XmlRoot),
		DescriptionAttr = xmerl_xpath:string (ChanXPath ++ "/@description", XmlRoot),
		if length (NameAttr) >  0 -> {xmlAttribute, name, _, _, _, _, _, _, Name, _} = hd (NameAttr)
		 ; length (NameAttr) =< 0 -> Name = "", exit ("'channel' is missing required 'name'")
		end,
		if length (IntervalAttr) >  0 -> {xmlAttribute, interval, _, _, _, _, _, _, Interval, _} = hd (IntervalAttr)
		 ; length (IntervalAttr) =< 0 -> Interval = ""
		end,
		if length (DescriptionAttr) >  0 -> {xmlAttribute, description, _, _, _, _, _, _, Description, _} = hd (DescriptionAttr)
		 ; length (DescriptionAttr) =< 0 -> Description = ""
		end,
		{Name, time:parse_interval (Interval), Description}
	catch
		exit:Desc -> error ({"parsing 'channel'", Entry, Desc})
	end.

% parse and return Entry through LastEntry of the ListXPath list's channels of the configuration XmlRoot
parse_channels (XmlRoot, ListXPath, Entry, LastEntry) ->
	if Entry >= LastEntry -> []; Entry < LastEntry ->
	[parse_channel (XmlRoot, ListXPath, Entry)] ++ parse_channels (XmlRoot, ListXPath, Entry + 1, LastEntry)
	end.

% parse and return the Entry list of the configuration XmlRoot
parse_list (XmlRoot, Entry, DefInter) ->
	try
		ListXPath = "/youtubemonitor/list[" ++ integer_to_list (Entry + 1) ++ "]",
		NameAttr        = xmerl_xpath:string (ListXPath ++ "/@name",        XmlRoot),
		DescriptionAttr = xmerl_xpath:string (ListXPath ++ "/@description", XmlRoot),
		if length (NameAttr) >  0 -> {xmlAttribute, name, _, _, _, _, _, _, Name, _} = hd (NameAttr)
		 ; length (NameAttr) =< 0 -> Name = "", exit ("'list' is missing required 'name'")
		end,
		if length (DescriptionAttr) >  0 -> {xmlAttribute, description, _, _, _, _, _, _, Description, _} = hd (DescriptionAttr)
		 ; length (DescriptionAttr) =< 0 -> Description = ""
		end,
		ChannelNodeCountRaw  = xmerl_xpath:string ("count(" ++ ListXPath ++ "/channel)",   XmlRoot),
		HiglightNodeCountRaw = xmerl_xpath:string ("count(" ++ ListXPath ++ "/highlight)", XmlRoot),
		{xmlObj, number, ChannelCount}   = ChannelNodeCountRaw,
		{xmlObj, number, HighlightCount} = HiglightNodeCountRaw,
		Channels   = parse_channels   (XmlRoot, ListXPath, 0, ChannelCount),
		Highlights = parse_highlights (XmlRoot, ListXPath, 0, HighlightCount),
		{ Name, Description, get_lowest_interval (Channels, DefInter), Channels, Highlights }
	catch
		exit:Desc -> error ({"parsing 'list'", Entry, Desc})
	end.

% parse and return Entry through LastEntry of the lists of the configuration XmlRoot
parse_lists (XmlRoot, Entry, LastEntry, DefInter) ->
	if Entry >= LastEntry -> []; Entry < LastEntry ->
	[parse_list (XmlRoot, Entry, DefInter)] ++ parse_lists (XmlRoot, Entry + 1, LastEntry, DefInter)
	end.

% parse and return the raw configuration Data string
parse_config (Data) ->
	try
		{XmlRoot, _RemainingText} = xmerl_scan:string (Data, [{validation, false}]),
		DataPnAttr   = xmerl_xpath:string ("/youtubemonitor/paths/@data",  XmlRoot),
		CachePnAttr  = xmerl_xpath:string ("/youtubemonitor/paths/@cache", XmlRoot),
		DefInterAttr = xmerl_xpath:string ("/youtubemonitor/defaults/@interval", XmlRoot),
		DefListAttr  = xmerl_xpath:string ("/youtubemonitor/defaults/@list",     XmlRoot),
		if length (DataPnAttr) >  0 -> {xmlAttribute, data, _, _, _, _, _, _, DataPn, _} = hd (DataPnAttr)
		 ; length (DataPnAttr) =< 0 -> DataPn = "", exit ("'paths' is missing required 'data'")
		end,
		if length (CachePnAttr) >  0 -> {xmlAttribute, cache, _, _, _, _, _, _, CachePn, _} = hd (CachePnAttr)
		 ; length (CachePnAttr) =< 0 -> CachePn = "", exit ("'paths' is missing required 'cache'")
		end,
		if length (DefInterAttr) >  0 -> {xmlAttribute, interval, _, _, _, _, _, _, DefInterStr, _} = hd (DefInterAttr)
		 ; length (DefInterAttr) =< 0 -> DefInterStr = "1m"
		end,
		if length (DefListAttr) >  0 -> {xmlAttribute, list, _, _, _, _, _, _, DefList, _} = hd (DefListAttr)
		 ; length (DefListAttr) =< 0 -> DefList = ""
		end,
		DefInter = time:parse_interval (DefInterStr),
		ListNodeCountRaw = xmerl_xpath:string ("count(/youtubemonitor/list)", XmlRoot),
		{xmlObj, number, ListCount} = ListNodeCountRaw,
		if ListCount >  0 -> ok
		 ; ListCount =< 0 -> exit ("there must be at least one 'list'")
		end,
		Lists = parse_lists (XmlRoot, 0, ListCount, DefInter),
		{{DataPn, CachePn, DefInter, DefList, get_lowest_global_interval (Lists, DefInter)}, Lists}
	catch
		exit:Desc -> error ({"parsing configuration", Desc})
	end.

% fetch and return the configuration in the file at Pathname
get_config (Pathname) ->
	RawConfig = case file:read_file (Pathname) of
		{ok, FileData} -> binary_to_list (FileData);
		Error          -> error ({"reading config file", Pathname, Error}), ""
	end,
	parse_config (RawConfig).

% eof
