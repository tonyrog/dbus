%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Control spotify / media player
%%% @end
%%% Created : 19 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_spotify).

-export([open/1]).
-export([open1/1]).

%% check codings playlist ...?
open1(Track) when is_list(Track) ->
    {ok, C} = dbus_connection:open(session),
    Fs = [{path,"/org/mpris/MediaPlay2"},
	  {destination, "org.mpris.MediaPlayer2.spotify"}],
    Uri = "string:spotify:track:"++Track,
    org_mpris_mediaplayer2_player:open_uri(C, Fs, Uri).

open(Track) when is_list(Track) -> 
    Uri = "string:spotify:track:"++Track,
    Destination = "org.mpris.MediaPlayer2.spotify",
    Interface   = "org.mpris.MediaPlayer2.Player",
    ObjectPath  = "/org/mpris/MediaPlayer2",
    Call        = Interface ++ ".OpenUri",
    os:cmd(string:join(
	     ["dbus-send",
	      "--session",
	      "--print-reply",
	      "--dest="++Destination,
	      ObjectPath,
	      Call,
	      Uri], " ")).
