% YouTube Monitor -> rss feed handling
% Yarin Licht March 16 2013
-module(rssf).

-export([connect_rss/0, disconnect_rss/1, get_channel_rss/3, rss_to_vidlist/1]).

% establish and return a connection to the place we're to fetch the rss feeds from
connect_rss () ->
	{"gdata.youtube.com", netw:connect_tcp (netw:get_ip_from_dn ("gdata.youtube.com"), 80)}.

% disconnect from the host, as connected by connect_rss/0
disconnect_rss (Connection) ->
	{_Dname, Socket} = Connection,
	netw:disconnect_tcp (Socket).

% download and return the rss feed of EntryCount for Channel from Addr
get_channel_rss (Addr, Channel, EntryCount) ->
	if EntryCount =< 0 -> QueryStr = ""
	 ; EntryCount >  0 -> QueryStr = "&start-index=1&max-results=" ++ integer_to_list (EntryCount)
	end,
	{Dname, Ip} = Addr,
	netw:get_http_doc (Ip, Dname, "/feeds/api/users/" ++ Channel ++ "/uploads?v=2.1" ++ QueryStr, []).

% parses and returns the xml text from XPath from XmlRoot
get_xpath_text (XmlRoot, XPath) ->
	get_xpath_text (xmerl_xpath:string (XPath ++ "/text()", XmlRoot)).
get_xpath_text (IdNode) ->
	{xmlText, _, _, _, NodeText, text} = hd (IdNode),
	if length (IdNode) =< 1 -> NodeText
	 ; length (IdNode) >  1 -> NodeText ++ get_xpath_text (tl (IdNode))
	end.

% parses and returns the Entry index rss feed from XmlRoot with Meta helper data
get_rss_entry (XmlRoot, Meta, Entry) ->
	XPathLead = "/feed/entry[" ++ integer_to_list (Entry + 1) ++ "]/media:group",
	IdUrl  = get_xpath_text (XmlRoot, XPathLead ++ "/yt:videoid"),
	Date   = get_xpath_text (XmlRoot, XPathLead ++ "/yt:uploaded"),
	Name   = get_xpath_text (XmlRoot, XPathLead ++ "/media:title"),
	{Author, AId} = Meta,
	Id = string:substr (IdUrl, string:rstr (IdUrl, "/") + 1),

	DurationNode = xmerl_xpath:string (XPathLead ++ "/yt:duration/@seconds", XmlRoot),
	{xmlAttribute, seconds, _, _, _, _, _, _, DurationStr, _} = hd (DurationNode),
	Length = time:to_pretty_duration (list_to_integer (DurationStr)),

	{Id, Name, Author, AId, Length, Date}.

% parses and returns the Entry to LastEntry index rss feeds from XmlRoot with Meta helper data
get_rss_entries (XmlRoot, Meta, Entry, LastEntry) ->
	if Entry >= LastEntry -> []; Entry < LastEntry ->
	[get_rss_entry (XmlRoot, Meta, Entry)] ++ get_rss_entries (XmlRoot, Meta, Entry + 1, LastEntry) 
	end.

% returns the number of rss feed entries there are in XmlRoot
get_rss_entry_count (XmlRoot) ->
	IdNodeCount = xmerl_xpath:string ("count(/feed/entry)", XmlRoot),
	{xmlObj, number, NodeCount} = IdNodeCount,
	NodeCount.

% parses and returns the rss file Data to and as a list of videos
rss_to_vidlist (Data) ->
	{XmlRoot, _RemainingText} = xmerl_scan:string (Data, [{validation, false}]),
	IdStr = get_xpath_text (XmlRoot, "/feed/id"),
	IdAuRight = string:substr (IdStr, string:str (IdStr, ":user:") + 6),
	Author = string:substr (IdAuRight, 1, string:rstr (IdAuRight, ":uploads") - 1),
	AIdAttr = xmerl_xpath:string ("/feed/link[@rel='alternate']/@href", XmlRoot),
	{xmlAttribute, href, _, _, _, _, _, _, ChanStr, _} = hd (AIdAttr),
	AIdChanStart = string:str (ChanStr, "/channel/UC") + 11,
	AId = string:substr (ChanStr, AIdChanStart, string:rstr (ChanStr, "/videos") - AIdChanStart),
	Entries = get_rss_entries (XmlRoot, {Author, AId}, 0, get_rss_entry_count (XmlRoot)),
	if length (Entries) =< 0 -> [{"<none>", "<none>", Author, AId, 0, calendar:universal_time ()}]
	 ; length (Entries) >  0 -> Entries
	end.

% eof
