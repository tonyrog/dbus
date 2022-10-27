%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Control spotify / media player
%%% @end
%%% Created : 19 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_spotify).

-export([open/1]).
-export([open1/1]).
%%
-export([test/0]).


open1(Data) ->
    {ok, C} = dbus_connection:open(session),
    Fs = [{path,"/org/mpris/MediaPlay2"},
	  {destination, "org.mpris.MediaPlayer2.spotify"}],
    Uri = object(Data),
    org_mpris_mediaplayer2_player:open_uri(C, Fs, Uri).

open(Data) -> %% require dbus-utils:  dbus-send
    Uri = object(Data),
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

object(Data) ->
    case string:trim(Data) of
	"string:spotify:track:"++ID -> track(ID);
	"track:"++ID -> track(ID);
	"string:spotify:album:"++ID -> album(ID);
	"album:"++ID -> album(ID);
	"string:spotify:playlist:"++ID -> playlist(ID);
	"playlist:"++ID -> playlist(ID);
	"string:spotify:episode:"++ID -> episode(ID);
	"episode:"++ID -> episode(ID);
	"string:spotify:show:"++ID -> show(ID);
	"show:"++ID -> show(ID);
	"string:spotify:user:"++ID -> user(ID);
	"user:"++ID -> user(ID);
	Data1 ->
	    case uri_string:parse(Data1) of
		#{ path := "/track/"++ID } -> track(ID);
		#{ path := "/album/"++ID } -> album(ID);
		#{ path := "/playlist/"++ID } -> playlist(ID);
		#{ path := "/episode/"++ID } -> episode(ID);
		#{ path := "/show/"++ID} -> show(ID);
		#{ path := "/user/"++ID} -> user(ID)
	    end
    end.

track(ID) -> "string:spotify:track:"++ID.
album(ID) -> "string:spotify:album:"++ID.
playlist(ID) -> "string:spotify:playlist:"++ID.
episode(ID) -> "string:spotify:episode:"++ID.
show(ID) -> "string:spotify:show:"++ID.
user(ID) -> "string:spotify:user:"++ID.

test() ->
    ok = test("https://open.spotify.com/track/58WAARD3a60af3yU5mkKwS?si=8a4150690a9f4d24"),
    ok = test("https://open.spotify.com/album/6vcgTVbCGHyVEWedGnpHL5?si=Ykto3rLqQuKN0rVeU8_XnA"),
    ok = test("https://open.spotify.com/show/1MSnJsiypViOsh6XnnzOVn?si=b06027d8412843cd"),
    ok = test("https://open.spotify.com/episode/4iCWReCqpscoTbCCSClIRu?si=76a59a549ae74b1c"),
    ok = test("https://open.spotify.com/playlist/37i9dQZF1E4A7OIobLeqTt?si=14bf3badb7684833"),
    ok = test("https://open.spotify.com/user/tonyrog?si=da6fc27512574553"),
    ok.


test(Url) ->
    Data1 = object(Url),
    Data1 = object(Data1),
    "string:spotify:"++Data2 = Data1,
    Data1 = object(Data2),
    ok.

    



    
