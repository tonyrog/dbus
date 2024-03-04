%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Control spotify / media player
%%% @end
%%% Created : 19 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_spotify).

-export([start/0]).
-export([open/1]).
-export([raise/0, quit/0]).
-export([play/0, pause/0]).
-export([stop/0, next/0, previous/0, seek/1, set_position/2]).
%%
-compile(export_all).
-export([test/0]).
-export([open_dbus_send/1]).

-type dbus_addr() :: pid() | string() | session |
      {Address::string(),AuthType::external|cookie|detect,
      Hello::boolean()}.

-define(DESTINATION, "org.mpris.MediaPlayer2.spotify").
-define(INTERFACE, "org.mpris.MediaPlayer2.Player").
-define(MPLAYER,   "/org/mpris/MediaPlayer2").

start() ->
    Result = os:cmd("(spotify 1>/dev/null 2>&1 &)"),
    timer:sleep(3000),
    Result.

get_playback_status() -> get_prop("PlaybackStatus").
get_loop_status()     -> get_prop("LoopStatus").
get_rate()            -> get_prop("Rate").
get_shuffle()         -> get_prop("Shuffle").
get_meta_data()       -> get_prop("MetaData").
get_volume()          -> get_prop("Volume").
get_position()        -> get_prop("Position").

get_prop(Prop) ->
    get_prop(session, Prop).
get_prop(Dbus, Prop) ->
    get_prop(Dbus,[{path,?MPLAYER},{destination,?DESTINATION}], Prop).
get_prop(Dbus,Fs,Name) ->
    get_prop(Dbus,Fs,?INTERFACE,Name).

get_prop(Con,Fs,Interface,Name) when is_pid(Con) ->
    case org_freedesktop_dbus_properties:get(Con,Fs,Interface,Name) of
	{ok,[{_Signature,Value}]} -> %% multi value return?
	    {ok,Value};
	Other ->
	    Other
    end;
get_prop(Address,Fs,Interface,Name) ->
    {ok,Con} = dbus_connection:open(Address),
    R = get_prop(Con,Fs,Interface,Name),
    dbus_connection:close(Con),
    R.

set_prop(Prop,Value) ->
    set_prop(session,Prop,Value).
set_prop(Dbus,Prop,Value) ->
    set_prop(Dbus,[{path,?MPLAYER},{destination,?DESTINATION}],Prop,Value).
set_prop(Dbus,Fs,Name,Value) ->
    set_prop(Dbus,Fs,?INTERFACE,Name,Value).

set_prop(Con,Fs,Interface,Name,Value) when is_pid(Con) ->
    org_freedesktop_dbus_properties:set(Con,Fs,Interface,Name,Value);
set_prop(Address,Fs,Interface,Name,Value) ->
    {ok,Con} = dbus_connection:open(Address),
    SigVal = if tuple_size(Value) =:= 2 -> Value;
	        is_float(Value) -> {"d", Value};
	        is_boolean(Value) -> {"b",Value};
	        is_list(Value) -> {"s", Value}
             end,
    R = set_prop(Con,Fs,Interface,Name,SigVal),
    dbus_connection:close(Con),
    R.


get_all_prop() ->
    get_all_prop(session).
get_all_prop(Dbus) ->
    get_all_prop(Dbus,[{path,?MPLAYER},{destination,?DESTINATION}]).
get_all_prop(Dbus,Fs) ->
    get_all_prop(Dbus,Fs,?INTERFACE).

get_all_prop(Con,Fs,Interface) when is_pid(Con) ->
    org_freedesktop_dbus_properties:get_all(Con,Fs,Interface);
get_all_prop(Address,Fs,Interface) ->
    {ok,Con} = dbus_connection:open(Address),
    R = get_all_prop(Con,Fs,Interface),
    dbus_connection:close(Con),
    R.

%% open uri
open(Data) -> open_uri(session, object(Data)).

open_uri(Uri) -> call(fun open_uri_fun/3, [Uri]).
open_uri(Addr, Uri) -> call(Addr, fun open_uri_fun/3, [Uri]).

open_uri_fun(Con,Fs,[Uri]) ->
    org_mpris_mediaplayer2_player:open_uri(Con, Fs, Uri).

%%
%%
%%
-spec play() -> ok | {error,Reason::term()}.
-spec play(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
play() -> call(fun play_fun/3).
play(Addr) -> call(Addr, fun play_fun/3, []).

play_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2_player:play(Con, Fs).
%%
%%
%%
-spec pause() -> ok | {error,Reason::term()}.
-spec pause(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
pause() -> call(fun pause_fun/3).
pause(Addr) -> call(Addr, fun pause_fun/3, []).

pause_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2_player:pause(Con, Fs).


-spec stop() -> ok | {error,Reason::term()}.
-spec stop(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
stop() -> call(fun stop_fun/3).
stop(Addr) -> call(Addr, fun stop_fun/3, []).
stop_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2_player:stop(Con, Fs).

-spec next() -> ok | {error,Reason::term()}.
-spec next(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
next() -> call(fun next_fun/3).
next(Addr) -> call(Addr, fun next_fun/3, []).

next_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2_player:next(Con, Fs).


-spec previous() -> ok | {error,Reason::term()}.
-spec previous(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
previous() -> call(fun previous_fun/3).
previous(Addr) -> call(Addr, fun previous_fun/3, []).

previous_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2_player:previous(Con, Fs).

-spec seek(Offset::integer()) -> ok | {error,Reason::term()}.
seek(Offset) -> call(fun seek_fun/3, [Offset]).
-spec seek(Addr::dbus_addr(),Offset::integer()) -> ok | {error,Reason::term()}.
seek(Addr,Offset) -> call(Addr, fun seek_fun/3, [Offset]).

seek_fun(Con,Fs,[Offset]) ->
    org_mpris_mediaplayer2_player:seek(Con, Fs, Offset).


-spec set_position(TrackId::string(),Position::integer()) -> ok | {error,Reason::term()}.
-spec set_position(Addr::dbus_addr(),TrackId::string(),Position::integer()) ->
      ok | {error,Reason::term()}.
set_position(TrackId,Position) ->
    call(fun set_position_fun/3, [TrackId,Position]).
set_position(Addr,TrackId,Position) ->
    call(Addr,fun set_position_fun/3, [TrackId,Position]).      

set_position_fun(Con,Fs,[TrackId,Postion]) ->
    org_mpris_mediaplayer2_player:set_position(Con, Fs, TrackId,Postion).

-spec raise() -> ok | {error,Reason::term()}.
-spec raise(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
raise() -> call(fun raise_fun/3).
raise(Addr) -> call(Addr, fun raise_fun/3, []).

raise_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2:raise(Con, Fs).

-spec quit() -> ok | {error,Reason::term()}.
-spec quit(Addr::dbus_addr()) -> ok | {error,Reason::term()}.
quit() -> call(fun quit_fun/3).
quit(Addr) -> call(Addr, fun quit_fun/3, []).

quit_fun(Con,Fs,_Args) ->
    org_mpris_mediaplayer2:quit(Con, Fs).
    

%% call media player
call(Fun) -> call(session, Fun, []).
call(Fun,Args) -> call(session, Fun, Args).
call(Addr, Fun, Args) ->
  call(Addr,[{path,?MPLAYER},{destination, ?DESTINATION}], Fun, Args).

call(Con, Fs, Fun, Args) when is_pid(Con) ->
    Fun(Con, Fs, Args);
call(Address, Fs, Fun, Args) ->
    {ok,Con} = dbus_connection:open(Address),
    R = call(Con, Fs, Fun, Args),
    dbus_connection:close(Con),
    R.

%% test     
open_dbus_send(Data) -> %% require dbus-utils:  dbus-send
    Uri = object(Data),
    Destination = ?DESTINATION,
    Interface   = "org.mpris.MediaPlayer2.Player",
    ObjectPath  = ?MPLAYER,
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

test_play_list() ->
  test_play_list(10000).
test_play_list(Interval) ->
  {ok,Con} = dbus_connection:open(session),
  lists:foreach(
    fun(Uri) ->
      open_uri(Con, object(Uri)),
      timer:sleep(3000),
      play(Con),
      timer:sleep(Interval)
    end, play_list()),
  stop(Con),
  dbus_connection:close(Con),
  ok.
  

play_list() ->
  [
  "https://open.spotify.com/track/1Q8n7UU4pULe4Mf1m3DxCm?si=1c4f87504d994fc8",
  "https://open.spotify.com/track/1h7iXjtC9X7V8VjGUiulri?si=56b7c94fdcde493a",
  "https://open.spotify.com/track/6b193f6NTdtHy2AosF0zTX?si=ff22c10798e24ea7",
  "https://open.spotify.com/track/6VCqx14OVaX9uwSvC0PvAG?si=3c080155963d4756",
  "https://open.spotify.com/track/4yjlWacI4vXSwtE7Kq7rUH?si=17403501932946f1",
  "https://open.spotify.com/track/0YOH3UcbN3rsB7pXWhvUQ4?si=bcfa9de1d2944b02",
  "https://open.spotify.com/track/2nTsKOXIVGDf2iPeVQO2Gm?si=2058fadc7a1245e2",
  "https://open.spotify.com/track/1krBKWusytTAT2jNWJGA7m?si=625779c60c4e4c04",
  "https://open.spotify.com/track/7GEHVYe7s1HL7m5KtEWeo9?si=c234f20f6f7c4e07"
  ].
  
