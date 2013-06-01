% YouTube Monitor -> main
% Yarin Licht March 16 to 24 2013
-module(ytmd).
-behaviour(application).

-export([start/2, start_link/2, stop/1, main_app/1, main/1, update_lists/1]).

% re-escape and return escaped characters in Str
c_escape (Str) ->
	Char = hd (Str),
	Sub = if Char =/= 92 -> [Char]
	       ; Char =:= 92 -> "\\\\" ++ [Char]
	      end,
	if length (Str) =< 1 -> Sub
	 ; length (Str) >  1 -> Sub ++ c_escape (tl (Str))
	end.

% push out Str a logger might want to know about
stat_echo (_Str) ->
	%io:format (Str ++ "\n"),
	ok.

% push out Str the user might want to know about, using Config
user_echo (Str, Config) ->
	{{DataPn, _CachePn, _DefInter, _DefList, _LowestInter}, _ListConfigs} = Config,
	_OSCmd = "kdialog --passivepopup \"" ++ c_escape (Str) ++ "\" 5 --title \"YouTube Monitor\" --icon \"" ++ DataPn ++ "/logo.jpg\"",
	% for if you want to be notified in KDE : stat_echo (Str), os:cmd (OSCmd), ok.
	stat_echo (Str).

% make note of un unhandled received mailbox Msg, From someone
default_msg (From, Msg) ->
	stat_echo (From ++ ": Don't know how to handle message: " ++ lists:flatten (io_lib:format ("~p", [Msg]))).

% sort and return a list of Vids, with the newest first, oldest last
sort_vidlist (Vids) ->
	lists:sort (fun compare_video_dates/2, Vids).
compare_video_dates (X, Y) ->
	{_XId, _XName, _XAuthor, _XAId, _XLength, XDate} = X,
	{_YId, _YName, _YAuthor, _YAId, _YLength, YDate} = Y,
	if XDate =< YDate -> false
	 ; XDate >  YDate -> true
	end.

% return a boolean value indicating whether or not Channel, with html output
% located at ChannelPn, is current, using the Connection and Config
check_channel_currency (Channel, ChannelPn, Connection, Config) ->
	FCFId = try
		{ok, RawCurrentData} = file:read_file (ChannelPn),
		Current = rssf:rss_to_vidlist (binary_to_list (RawCurrentData)),
		{FileCurrentFirstId, _, _, _, _, _} = hd (Current),
		FileCurrentFirstId
	catch
		exit:_Desc  -> false;
		error:_Desc -> false
	end,
	CurrentFirstId = if (FCFId =:= true) orelse (FCFId =:= false) -> FCFId; true ->
		{{_DataPn, _CachePn, DefInter, _DefList, _LowestInter}, ListConfigs} = Config,
		InterSecs = conf:get_broad_specific_interval (ListConfigs, DefInter, Channel),
		CurrSecs = calendar:datetime_to_gregorian_seconds (calendar:universal_time ()),
		DestSecs = calendar:datetime_to_gregorian_seconds (time:get_file_time (ChannelPn)) + InterSecs,
		%DecayProg = ((DestSecs - CurrSecs) / InterSecs) * 100,
		%echo (Channel ++ " has " ++ integer_to_list (round (DecayProg)) ++ "% of it's update interval (" ++ integer_to_list (InterSecs) ++ " seconds) remaining"),
		if CurrSecs >= DestSecs -> FCFId
		 ; CurrSecs <  DestSecs -> true
		end
	end,
	if (CurrentFirstId =:= true) orelse (CurrentFirstId =:= false) -> CurrentFirstId
	 ; length (CurrentFirstId) =:= 0 -> false
	 ; length (CurrentFirstId) >   0 ->
		%echo ("Checking " ++ Channel ++ " currency online..."),
		NewCheckData = rssf:get_channel_rss (Connection, Channel, 1),
		NewCheck = rssf:rss_to_vidlist (NewCheckData),
		{NewCheckFirstId, _, _, _, _, _} = hd (NewCheck),
		FileMatch = string:equal (CurrentFirstId, NewCheckFirstId),
		if FileMatch =:= true  -> ok
		 ; FileMatch =:= false -> file:delete (ChannelPn)
		end,
		FileMatch
	end.

% make sure a Channel is current, using the Connection and Config,
% returning whether or not it's unparsed cached copy has been updated or not
ensure_channel_currency (Channel, Connection, Config) ->
	{{_DataPn, CachePn, _DefInter, _DefList, _LowestInter}, _ListConfigs} = Config,
	ChannelPn   = CachePn ++ "/feeds/"    ++ netw:urlencode (Channel) ++ ".xml",
	ChanCachePn = CachePn ++ "/channels/" ++ netw:urlencode (Channel) ++ ".html",
	ChanTermsPn = CachePn ++ "/feeds/"    ++ netw:urlencode (Channel) ++ ".terms",
	Currency = check_channel_currency (Channel, ChannelPn, Connection, Config),
	if Currency =:= true  ->
		HtmlExists = filelib:is_regular (ChanCachePn) and filelib:is_regular (ChanTermsPn),
		if HtmlExists =:= true  -> uptodate
		 ; HtmlExists =:= false -> uptodate_unparsed
		end
	 ; Currency =:= false ->
		NewData = rssf:get_channel_rss (Connection, Channel, 20),
		case file:write_file (ChannelPn, list_to_binary (NewData)) of
			ok              -> updated;
			{error, Reason} -> stat_echo ({"updating feed file", ChannelPn, Reason})
		end
	end.

% makes sure Channel of ListConfig of Config is up to date,
% using Connection if needed, and returning whether or not an update occured
update_channel (Channel, Connection, ListConfig, Config) ->
	Update = ensure_channel_currency (Channel, Connection, Config),
	{{_DataPn, CachePn, _DefInter, _DefList, _LowestInter}, _ListConfigs} = Config,
	ChannelPn   = CachePn ++ "/feeds/"    ++ netw:urlencode (Channel) ++ ".xml",
	ChanCachePn = CachePn ++ "/channels/" ++ netw:urlencode (Channel) ++ ".html",
	ChanTermsPn = CachePn ++ "/feeds/"    ++ netw:urlencode (Channel) ++ ".terms",
	if (Update =:= updated) orelse (Update =:= uptodate_unparsed) ->
		{ok, RawChannelData} = file:read_file (ChannelPn),
		ChannelVids = rssf:rss_to_vidlist (binary_to_list (RawChannelData)),
		case file:write_file (ChanTermsPn, term_to_binary (ChannelVids)) of
			ok               -> ok;
			{error, ReasonT} -> stat_echo ({"updating terms file", ChanTermsPn, ReasonT})
		end,
		ChannelHtml = html:html_format_channel (sort_vidlist (ChannelVids), Config, ListConfig),
		case file:write_file (ChanCachePn, list_to_binary (html:htmlent_superlatin (ChannelHtml))) of
			ok               -> ok;
			{error, ReasonC} -> stat_echo ({"updating channel file", ChanCachePn, ReasonC})
		end,
		stat_echo (Channel ++ " channel has been updated"),
		{updated, ChannelVids}
	 ; Update =:= uptodate ->
		{ok, RawCachedVidlistData} = file:read_file (ChanTermsPn),
		CachedVidlist = binary_to_term (RawCachedVidlistData),
		stat_echo (Channel ++ " channel is up to date"),
		{uptodate, CachedVidlist}
	end.

% makes sure all of the Channels of ListConfig of Config are up to date,
% using Connection, re-establishing it if needed, and returning whether or not an update occured
update_channels (Connection, Channels, ListConfig, Config) ->
	{Name, _Interval, _Description} = hd (Channels),
	Uptup2 = try % forgive a failure, but only once
		update_channel (Name, Connection, ListConfig, Config)
	catch
		exit:Desc -> case Desc of
			{"getting", _} = Desc -> reconnect
			; _ -> exit (Desc)
		end
	end,
	Uptup = if Uptup2 =:= reconnect ->
		stat_echo ("There was a problem loading an RSS feed; will try again..."),
		rssf:disconnect_rss (Connection),
		RecentConnection = rssf:connect_rss (),
		update_channel (Name, RecentConnection, ListConfig, Config)
	 ; Uptup2 =/= reconnect ->
		RecentConnection = Connection,
		Uptup2
	end,
	{Update, Compilation} = Uptup,
	if length (Channels) =< 1 -> Uptup
	 ; length (Channels) >  1 ->
		{NextUpdate, NextCompilation} = update_channels (RecentConnection, tl (Channels), ListConfig, Config),
		LeaveUpdate = if Update =:= updated  -> updated
		               ; Update =:= uptodate -> NextUpdate
		              end,
		{LeaveUpdate, Compilation ++ NextCompilation}
	end.

% makes sure all of the channels of ListConfig of Config are up to date,
% establishing an connection with the remote server for possible use, and returning whether or not an update occured
update_channels (ListConfig, Config) ->
	{_Name, _Desc, _LowestInter, Channels, _Highlights} = ListConfig,
	Connection = rssf:connect_rss (),
	try
		update_channels (Connection, Channels, ListConfig, Config)
	after
		rssf:disconnect_rss (Connection)
	end.

% makes sure the list of ListConfig of Config is up to date, returning whether or not an update occured
update_list (ListConfig, Config) ->
	{{_DataPn, CachePn, _DefInter, _DefList, _LowestInter}, _ListConfigs} = Config,
	{ListName, _ListDesc, ListLowestInter, _ListChannels, _ListHighlights} = ListConfig,
	ListCachePn = CachePn ++ "/lists/" ++ netw:urlencode (ListName) ++ ".html",
	CurrSecs = calendar:datetime_to_gregorian_seconds (calendar:universal_time ()),
	DestSecs = calendar:datetime_to_gregorian_seconds (time:get_file_time (ListCachePn)) + ListLowestInter,
	%DecayProg = ((DestSecs - CurrSecs) / ListLowestInter) * 100,
	%echo (ListName ++ " has " ++ integer_to_list (round (DecayProg)) ++ "% of it's update interval (" ++ integer_to_list (ListLowestInter) ++ " seconds) remaining"),
	if CurrSecs >= DestSecs ->
		{Update1, Compilation} = update_channels (ListConfig, Config),
		Update = if Update1 =:= updated -> updated; Update1 =:= uptodate ->
			HtmlExists = filelib:is_regular (ListCachePn),
			if HtmlExists =:= true  -> uptodate
			 ; HtmlExists =:= false -> updated
			end
		end,
		if Update =:= updated  ->
			ListHtml = html:html_format_list (lists:sublist (sort_vidlist (Compilation), 1, 80), Config, ListConfig),
			case file:write_file (ListCachePn, list_to_binary (html:htmlent_superlatin (ListHtml))) of
				ok              -> ok;
				{error, Reason} -> error ({"updating list file", ListCachePn, Reason})
			end,
			stat_echo (ListName ++ " list has been updated"),
			updated
		 ; Update =:= uptodate ->
			stat_echo (ListName ++ " list is up to date"),
			uptodate
		end
	 ; CurrSecs <  DestSecs ->
		stat_echo (ListName ++ " list is up to date"),
		uptodate
	end.

% makes sure all the lists in Config are up to date, returning whether or not an update occured
update_lists (Config) ->
	{_Pathnames, ListConfigs} = Config,
	exit (update_lists (ListConfigs, Config)).
update_lists (ListConfigs, Config) ->
	ThisList = update_list (hd (ListConfigs), Config),
	if length (ListConfigs) =< 1 -> ThisList
	 ; length (ListConfigs) >  1 ->
		NextList = update_lists (tl (ListConfigs), Config),
		if ThisList =:= updated  -> updated
		 ; ThisList =:= uptodate -> NextList
		end
	end.

% the daemon's updating supervisor via UpdatePid using Config
main_updating (UpdatePid, Config) ->
	receive
		{'EXIT', UpdatePid, updated}  ->
			{{_DataPn, CachePn, _DefInter, DefList, _LowestInter}, _ListConfigs} = Config,
			EchoStr = if length (DefList) >   0 -> "<a href=\"" ++ CachePn ++ "/lists/" ++ netw:urlencode (DefList) ++ ".html" ++ "\">A list has been updated</a>"
			           ; length (DefList) =:= 0 -> "A list has been updated"
			end,
			user_echo (EchoStr, Config), continue;
		{'EXIT', UpdatePid, uptodate} ->
			stat_echo ("All lists are up to update"), continue;
		{'EXIT', UpdatePid, shutdown} ->
			stat_echo ("List updating is to be aborted due to shutdown"), break;
		{'EXIT', UpdatePid, Reason}   ->
			user_echo ("An error occured when trying to update a list:\n" ++ lists:flatten (io_lib:format ("~p", [Reason])), Config), continue;
		{'EXIT', _Origin, shutdown}   ->
			stat_echo ("Shutdown sequence commenced..."), exit (UpdatePid, shutdown), main_updating (UpdatePid, Config), break;
		Msg                           ->
			default_msg ("main_updating", Msg), main_updating (UpdatePid, Config)
	end.

% the daemon's idling supervisor using Config
main_waiting (Config) ->
	{{_DataPn, _CachePn, _DefInter, _DefList, LowestInter}, _ListConfigs} = Config,
	receive
		{'EXIT', _Origin, shutdown} -> stat_echo ("Shutdown sequence commenced..."), break;
		Msg                         -> default_msg ("main_waiting", Msg), main_waiting (Config)
	after
		LowestInter * 1000          -> continue
	end.

% the daemon's main repeating loop using Config
main_loop (Config) ->
	UpdatePid = spawn_link (?MODULE, update_lists, [Config]),
	Update = main_updating (UpdatePid, Config),
	if Update =:= break    -> ok
	 ; Update =:= continue ->
		Wait = main_waiting (Config),
		if Wait =:= break    -> ok
		 ; Wait =:= continue -> main_loop (Config)
		end
	end.

% the program's actual entry point, with ConfPn
main (ConfPn) ->
	stat_echo ("YTMD started"),
	process_flag (trap_exit, true),
	Config = conf:get_config (ConfPn),
	stat_echo ("YTMD ready"),
	main_loop (Config),
	stat_echo ("YTMD stopped"),
	ok.

% the program's application entry point, with Params
main_app (_Params) ->
	{ok, ConfPn} = application:get_env (cfgfile),
	main (ConfPn).

% application start and stop callbacks
start      (_Mod, Args) -> {ok, spawn      (?MODULE, main_app, [Args])}.
start_link (_Mod, Args) -> {ok, spawn_link (?MODULE, main_app, [Args])}.
stop       (_State) -> ok.

% compile and run with...
%	c(time),c(conf),c(netw),c(rssf),c(text),c(html),c(ytmd). 
%	application:start(ytmd).
%	application:stop(ytmd).

% eof
