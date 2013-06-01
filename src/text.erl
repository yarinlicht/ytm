% YouTube Monitor -> textual formatting
% Yarin Licht March 16 2013
-module(text).

-export([text_format_vidlist/1]).

% textually format and return the list Vids as per the FormatSpecs
text_format_vidlist (Vids, FormatSpecs) ->
	{IdWidth, NameWidth, AuthorWidth, AIdWidth, LengthWidth, DateWidth} = FormatSpecs,
	{Id, Name, Author, AId, Length, Date} = hd (Vids),
	DateStr = time:to_pretty_time (time:from_isoex_time (Date)),
	Str = string:left  (Id,     IdWidth,     hd (" ")) ++ " " ++ string:left  (Name,    NameWidth, hd (" ")) ++ " " ++
	      string:left  (Author, AuthorWidth, hd (" ")) ++ " " ++ string:left  (AId,     AIdWidth,  hd (" ")) ++ " " ++
	      string:right (Length, LengthWidth, hd (" ")) ++ " " ++ string:right (DateStr, DateWidth, hd (" ")) ++ "\n",
	if length (Vids) =< 1 -> Str
	 ; length (Vids) >  1 -> Str ++ text_format_vidlist (tl (Vids), FormatSpecs)
	end.

% calculate and return some new text formatting specs for the list Vids as per the OldSpecs
pre_text_format_vidlist (Vids, OldSpecs) ->
	{OldIdWidth, OldNameWidth, OldAuthorWidth, OldAIdWidth, OldLengthWidth, OldDateWidth} = OldSpecs,
	{Id, Name, Author, AId, Length, Date} = hd (Vids),
	DateStr = time:to_pretty_time (time:from_isoex_time (Date)),
	if OldIdWidth >  length (Id) -> NewIdWidth = OldIdWidth
	 ; OldIdWidth =< length (Id) -> NewIdWidth = length (Id)
	end,
	if OldNameWidth >  length (Name) -> NewNameWidth = OldNameWidth
	 ; OldNameWidth =< length (Name) -> NewNameWidth = length (Name)
	end,
	if OldAuthorWidth >  length (Author) -> NewAuthorWidth = OldAuthorWidth
	 ; OldAuthorWidth =< length (Author) -> NewAuthorWidth = length (Author)
	end,
	if OldAIdWidth >  length (AId) -> NewAIdWidth = OldAIdWidth
	 ; OldAIdWidth =< length (AId) -> NewAIdWidth = length (AId)
	end,
	if OldLengthWidth >  length (Length) -> NewLengthWidth = OldLengthWidth
	 ; OldLengthWidth =< length (Length) -> NewLengthWidth = length (Length)
	end,
	if OldDateWidth >  length (DateStr) -> NewDateWidth = OldDateWidth
	 ; OldDateWidth =< length (DateStr) -> NewDateWidth = length (DateStr)
	end,
	NewSpecs = {NewIdWidth, NewNameWidth, NewAuthorWidth, NewAIdWidth, NewLengthWidth, NewDateWidth},
	if length (Vids) =< 1 -> NewSpecs
	 ; length (Vids) >  1 -> pre_text_format_vidlist (tl (Vids), NewSpecs)
	end.

% textually format and return the list Vids
text_format_vidlist (Vids) ->
	FormatSpecs = pre_text_format_vidlist (Vids, {8, 10, 12, 10, 6, 4}),
	{IdWidth, NameWidth, AuthorWidth, AIdWidth, LengthWidth, DateWidth} = FormatSpecs,
	Header = string:left  ("Video ID",     IdWidth,     hd (" ")) ++ " " ++ string:left  ("Video Name", NameWidth, hd (" ")) ++ " " ++
	         string:left  ("Channel Name", AuthorWidth, hd (" ")) ++ " " ++ string:left  ("Channel ID", AIdWidth,  hd (" ")) ++ " " ++
	         string:right ("Length",       LengthWidth, hd (" ")) ++ " " ++ string:right ("Date",       DateWidth, hd (" ")) ++ "\n",
	Separator = string:right ("", IdWidth + NameWidth + AuthorWidth + AIdWidth + LengthWidth + DateWidth + 5, hd ("-")) ++ "\n",
	{FirstVidId, _, _, _, _, _} = hd (Vids),
	FirstVidFake = string:equal (FirstVidId, "<none>"),
	if FirstVidFake =:= true  -> Header ++ Separator ++ "No videos" ++ Separator
	 ; FirstVidFake =:= false -> Header ++ Separator ++ text_format_vidlist (Vids, FormatSpecs) ++ Separator
	end.

% eof
