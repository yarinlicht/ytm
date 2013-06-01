% YouTube Monitor -> Networking
% Yarin Licht March 16 2013
-module(netw).

-export([get_ip_from_dn/1, connect_tcp/2, disconnect_tcp/1, get_http_doc/4, urlencode/1, htmlspecialchars/1]).

% try to parse some incompletely parsed chunked http Data
dechunk_data (Data) ->
	NextData = dechunk_data_once (Data),
	if NextData =:= Data -> Data
	 ; NextData =/= Data ->
		{_Creamy, _Chunky, Done} = NextData,
		if Done =:= true  -> NextData
		 ; Done =:= false -> dechunk_data (NextData)
		end
	end.
dechunk_data_once (Data) ->
	{Creamy, Chunky, false} = Data,
	if length (Creamy) =< 0 ->
		EohLoc = string:str (Chunky, "\r\n\r\n"),
		if EohLoc >  0 -> { string:substr (Chunky, 1, EohLoc + 3),
		                    string:substr (Chunky, EohLoc + 4), false }
		 ; EohLoc =< 0 -> Data
		end
	 ; length (Creamy) >  0 ->
		EolenLoc = string:str (Chunky, "\r\n"),
		if (EolenLoc >  0) andalso (EolenLoc <  20) ->
			LeadLen = list_to_integer (string:substr (Chunky, 1, EolenLoc - 1), 16),
			MaxChunkLen = length (Chunky) - (EolenLoc + 3),
			if LeadLen =< MaxChunkLen ->
				TrimmedChunky = string:substr (Chunky, EolenLoc + 2),
				"\r\n" = string:substr (TrimmedChunky, LeadLen + 1, 2),
				{ Creamy ++ string:substr (TrimmedChunky, 1, LeadLen),
					string:substr (TrimmedChunky, LeadLen + 3),
					LeadLen =:= 0 }
				; LeadLen >  MaxChunkLen -> Data
			end
			; (EolenLoc =< 0) orelse  (EolenLoc >= 20) -> Data
		end
	end.

% the number of milliseconds we're willing to wait on a networking
% operation before giving up
net_timeout () -> 5000.

% return a random, mainstream-looking http user agent string
random_ua_string () ->
	UAList = [
	"Mozilla/5.0 (Windows NT 6.1; rv:15.0) Gecko/20120716 Firefox/15.0a2",
	"Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)",
	"Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/525.13 (KHTML, like Gecko) Chrome/0.2.149.27 Safari/525.13",
	"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/534.16 (KHTML, like Gecko) Chrome/10.0.648.204 Safari/534.16",
	"Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/534.24 (KHTML, like Gecko) Chrome/11.0.696.16 Safari/534.24",
	"Mozilla/5.0 (Linux; U; Android 2.2; en-us; Nexus One Build/FRF91) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1",
	"Mozilla/5.0 (Linux; U; Android 2.2; en-us; GT-P1000 Build/FROYO) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1",
	"Mozilla/5.0 (iPad; U; CPU OS 3_2_1 like Mac OS X; en-us) AppleWebKit/531.21.10 (KHTML, like Gecko) Mobile/7B405"],
	lists:nth (random:uniform (length (UAList)), UAList).

% returns the ip address of Name
get_ip_from_dn (Name) ->
	case inet:getaddr (Name, inet) of
		{ok, Addr}      -> Addr;
		{error, Reason} -> error ({"resolving ip", Name, Reason, inet:format_error (Reason)})
	end.

% opens and returns a new tcp connection to Addr on Port
connect_tcp (Addr, Port) ->
	case gen_tcp:connect (Addr, Port, [{packet, raw}, {active, true}], net_timeout ()) of
		{ok, Socket}    ->
			inet:setopts (Socket, [{send_timeout, net_timeout () / 1000}, {linger, {true, net_timeout () / 1000}}]),
			Socket;
		{error, Reason} -> error ({"connecting", Reason, inet:format_error (Reason)})
	end.

% closes the tcp Connection in the Direction. closing both will free the system's resources
disconnect_tcp (Connection, Direction) ->
	case Direction of
		read  -> gen_tcp:shutdown (Connection, read), ok;
		write -> gen_tcp:shutdown (Connection, write), ok;
		both  -> gen_tcp:close (Connection), ok
	end.
disconnect_tcp (Connection) ->
	disconnect_tcp (Connection, both).

% sends data Packet down the Connection
send_tcp (Connection, Packet) ->
	case gen_tcp:send (Connection, list_to_binary (Packet)) of
		ok              -> ok;
		{error, Reason} -> error ({"sending", Reason, inet:format_error (Reason)})
	end.

% receives and returns data from the Connection one time
recv_tcp_once (_Connection) ->
	receive
		{tcp, _Socket, Data}         -> Data;
		{tcp_closed, _Socket}        -> error ({"receiving", "connection terminated"});
		{tcp_error, _Socket, Reason} -> error ({"receiving", Reason, inet:format_error (Reason)});
		Msg                          -> error ({"receiving", "unknown message", Msg})
	after
		net_timeout () -> error ({"receiving", timeout})
	end.

% receives and returns data from the Connection
recv_chunky_tcp (Connection, Data) ->
	{OldCreamy, OldChunky, false} = Data,
	NewData = dechunk_data ({OldCreamy, OldChunky ++ recv_tcp_once (Connection), false}),
	{NewCreamy, _NewChunky, NewDone} = NewData,
	if NewDone =:= true  -> NewCreamy
	 ; NewDone =:= false -> recv_chunky_tcp (Connection, NewData)
	end.
recv_chunky_tcp (Connection) ->
	recv_chunky_tcp (Connection, {[], [], false}).

% retrieves the document Pathname with EntityBody from the host Domain at the http server Addr
get_http_doc (Connection, Domain, Pathname, EntityBody) ->
	Request = "GET " ++ Pathname ++ " HTTP/1.1\r\nHost: " ++ Domain ++ "\r\n"
	          "User-Agent: " ++ random_ua_string () ++"\r\nTransfer-Encoding: chunked\r\n",
	if length (EntityBody) >  0 -> 
		RequestEnd = "Content-Length: " ++ integer_to_list (length (EntityBody)) ++ "\r\n"
		             "Content-Type: application/x-www-form-urlencoded; charset=UTF-8\r\n\r\n" ++ EntityBody
	 ; length (EntityBody) =< 0 -> RequestEnd = "\r\n"
	end,
	try
		send_tcp (Connection, Request ++ RequestEnd),
		Data = recv_chunky_tcp (Connection),
		"HTTP/1.1 200 OK" = string:substr (Data, 1, 15),
		string:substr (Data, string:str (Data, "\r\n\r\n") + 4)
	catch
		error:Desc -> exit ({"getting", Desc})
	end.

% prepare and return Str for use in a url segment
urlencode (Str) -> edoc_lib:escape_uri (Str).

% prepare and return Str for in html text
htmlspecialchars (Str) ->
	Rep = case [hd (Str)] of
		"&"  -> "&amp;";
		"\"" -> "&quot;";
		"'"  -> "&#039;";
		"<"  -> "&lt;";
		">"  -> "&gt;";
		X    -> X
	end,
	if length (Str) =< 1 -> Rep
	 ; length (Str) >  1 -> Rep ++ htmlspecialchars (tl (Str))
	end.

% eof
