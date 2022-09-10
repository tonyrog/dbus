%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%     Top level dbus constructs
%%% @end
%%% Created :  5 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(dbus).

-compile(export_all).

-include("../include/dbus.hrl").

%%
%% open a dbus_connection
%% the connection is authenticated and is hello negotiated automatically.
%% return {ok, Connection} | {error, Reason}
%%
open() ->
    dbus_connection:open().

open(Address) ->
    dbus_connection:open(Address).

close(C) ->
    dbus_connection:close(C).


%% this will setup a number of interfaces
setup() ->
    dbus_compile:builtin().

monitor_devices() ->
    {ok,C} = open(system),
    Path = "/org/freedesktop/DBus",
    Intf = "org.freedesktop.DBus",
    Rmap =
	add_matches(C, Path, Intf,
		    [
		     "eavesdrop=true,type='signal',member='DeviceAdded'",
		     "eavesdrop=true,type='signal',member='DeviceRemoved'"
		    ], #{}),
    monitor_loop(Rmap, 1).


monitor_buttons() ->
    {ok,C} = open(system),
    Path = "/org/gnome/Shell",
    Intf = "org.gnome.Shell",
    Rmap =
	add_matches(C, Path, Intf, 
		    [
		     "eavesdrop=true,type='method_call',member='ShowOSD'"
		    ], #{}),
    monitor_loop(Rmap, 1).

monitor_bluetooth() ->
    monitor([
	     "eavesdrop=true,type='method_call',path=/org/freedesktop/Notifications,member=Notify"
	    ]).

monitor_music() ->
    monitor([
	     "eavesdrop=true,type='signal',path=/org/mpris/MediaPlayer2,member=PropertiesChanged"
	    ]).
    

monitor_signals() ->
    monitor_signals("").
monitor_signals(Filter) ->
    monitor([append_filter("eavesdrop=true,type='signal'",Filter)]).

append_filter(Rule, "") -> Rule;
append_filter(Rule, Filter) -> Rule ++ "," ++ Filter.

monitor_calls() ->
    monitor([
	     "eavesdrop=true,type='method_call'",
	     "eavesdrop=true,type='method_return'"
	    ]).

monitor() ->
    monitor(["eavesdrop=true,type='method_call'",
	     "eavesdrop=true,type='method_return'",
	     "eavesdrop=true,type='signal'",
	     "eavesdrop=true,type='error'"]).

monitor(Rules) ->
    {ok,C} = open(),
    Path = "/org/freedesktop/DBus",
    Intf = "org.freedesktop.DBus",
    Rmap =
	add_matches(C, Path, Intf, Rules, #{}),
    monitor_loop(Rmap, 1).

add_matches(C, Path, Intf, [Rule|Rules], Acc) ->
    {ok,Ref} = add_match(C, Path, Intf, Rule),
    add_matches(C, Path, Intf, Rules, Acc#{ Ref => true });
add_matches(_C, _Path, _Intf, [], Acc) ->
    Acc.
    
add_match(C, Path, Intf, Rule) ->
    org_freedesktop_dbus:add_match(C,
				   [{destination,Intf},
				    {path,Path}],
				   Rule).

monitor_loop(Refs,I) ->
    receive
	{dbus_match, Ref, Header, Message} ->
	    case maps:get(Ref, Refs, false) of
		true ->
		    Fds = Header#dbus_header.fields,
		    io:format("~w: ~s sender=~s -> dest=~s serial=~w path=~s; interface=~s; member=~s\n~p\n\n", 
			      [I, 
			       Header#dbus_header.message_type,
			       Fds#dbus_field.sender,
			       Fds#dbus_field.destination,
			       Header#dbus_header.serial,
			       Fds#dbus_field.path,
			       Fds#dbus_field.interface,
			       Fds#dbus_field.member,
			       cstring(Message)
			      ]),
		    monitor_loop(Refs,I+1);
		false ->
		    monitor_loop(Refs,I)
	    end;
	{dbus_match_stop, Ref} ->
	    case maps:get(Ref,Refs,false) of
		true ->
		    ok;
		false ->
		    monitor_loop(Refs,I)
	    end;
	Other ->
	    io:format("GOT OTHER: ~p\n", [Other]),
	    monitor_loop(Refs,I)
    end.

%% remove terminating 0 in cstrings for nicer looking output
cstring([H|T]) -> [cstring(H)|cstring(T)];
cstring([]) -> [];
cstring(X) when is_tuple(X) -> list_to_tuple(cstring(tuple_to_list(X)));
cstring(X) when is_binary(X) ->
    S = byte_size(X)-1,
    case X of
	<<Y:S/binary,0>> -> Y;
	_ -> X
    end;
cstring(X) -> X.
