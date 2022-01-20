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

%% this will setup a number of interfaces
setup() ->
    dbus_compile:builtin().

monitor_devices() ->
    {ok,C} = open(system),
    Rs =
	add_matches(C, 
		    [
		     "eavesdrop=true,type='signal',member='DeviceAdded'",
		     "eavesdrop=true,type='signal',member='DeviceRemoved'"
		    ], []),
    monitor_loop(Rs).

monitor() ->
    {ok,C} = open(),
    Rs =
	add_matches(C, 
		    [
		     "eavesdrop=true,type='method_call'",
		     "eavesdrop=true,type='method_return'",
		     "eavesdrop=true,type='error'"
		    ], []),
    monitor_loop(Rs).

add_matches(C, [Rule|Rules], Acc) ->
    {ok,Ref} = add_match(C, Rule),
    add_matches(C, Rules, [Ref|Acc]);
add_matches(_C, [], Acc) ->
    Acc.
    
add_match(C, Rule) ->
    org_freedesktop_dbus:add_match(C,
				   [{destination,"org.freedesktop.DBus"},
				    {path,"/org/freedesktop/DBus"}],
				   Rule).

monitor_loop(Refs) ->
    receive
	{dbus_match, Ref, Header, Message} ->
	    case lists:member(Ref, Refs) of
		true ->
		    Fds = Header#dbus_header.fields,
		    io:format("~s sender=~s -> dest=~s serial=~w path=~s; interface=~s; member=~s\n~p\n", 
			      [Header#dbus_header.message_type,
			       Fds#dbus_field.sender,
			       Fds#dbus_field.destination,
			       Header#dbus_header.serial,
			       Fds#dbus_field.path,
			       Fds#dbus_field.interface,
			       Fds#dbus_field.member,
			       cstring(Message)
			      ]),
		    monitor_loop(Refs);
		false ->
		    monitor_loop(Refs)
	    end;
	{dbus_match_stop, Ref} ->
	    case lists:member(Ref,Refs) of
		true ->
		    ok;
		false ->
		    monitor_loop(Refs)
	    end;
	Other ->
	    io:format("GOT OTHER: ~p\n", [Other]),
	    monitor_loop(Refs)
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
