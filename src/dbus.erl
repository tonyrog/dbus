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

%% check for xml spec of interface
list_names() -> list_names(session).
list_names(Address) ->
    {ok, C} = dbus_connection:open(Address),
    R = org_freedesktop_dbus:list_names(C,[{destination,"org.freedesktop.DBus"},
					   {path,"/org/freedesktop/DBus"}]),
    dbus_connection:close(C),
    case R of
	{ok,[List]} ->
	    {ok, lists:sort([L || L <- List, hd(L) =/= $:])};
	_ -> R
    end.

get_all(Path, Interface) ->
    {ok, C} = dbus_connection:open(session),
    All = get_all(C, Path, Interface),
    dbus_connection:close(C),
    All.

get_all(C, Path, Interface) ->
    Fs = [{path,Path}],
    org_freedesktop_dbus_properties:get_all(C,Fs,Interface).

i() ->
    case list_names() of
	{ok, List} ->
	    lists:foreach(
	      fun(Interface) -> 
		      io:format("~s\n", [Interface])
	      end, List);
	Error ->
	    Error
    end.

%% construct a path form interface name "org.foo.bar" => /org/foo/bar
interface_to_path(Interface) ->
    binary_to_list(iolist_to_binary(
		     ["/"|re:replace(Interface, "\\.", "\\/", [global])])).

introspect(Interface) ->
    introspect(sesssion,interface_to_path(Interface), Interface).

introspect(Address, Interface) ->
    introspect(Address, interface_to_path(Interface), Interface).
    
introspect(Address, Path, Interface) ->
    {ok, C} = dbus_connection:open(Address),
    R = dbus_connection:call(C,
			     [{path, Path},
			      {destination, Interface},
			      {interface,
			       "org.freedesktop.DBus.Introspectable"},
			      {member, "Introspect"}],
			     "",
			     []),
    dbus_connection:close(C),
    R.

save_introspect(Interface, Filename) ->
    save_introspect(session, interface_to_path(Interface), Interface, Filename).
save_introspect(Address, Path, Interface, Filename) ->
    case introspect(Address, Path, Interface) of
	{ok, Xml} ->
	    file:write_file(Filename, Xml);
	Error ->
	    Error
    end.

monitor_devices() ->
    {ok,C} = open(system),

    MonFs = [{path, "/org/freedesktop/DBus"},
	     {destination, "org.freedesktop.DBus"}],
    Rmap =
	add_matches(C, MonFs,
		    [
		     "eavesdrop=true,type='signal',member='DeviceAdded'",
		     "eavesdrop=true,type='signal',member='DeviceRemoved'"
		    ], #{}),
    monitor_loop(C,Rmap, 1).


monitor_buttons() ->
    {ok,C} = open(system),
    MonFs = [{path, "/org/gnome/Shell"},
	     {destination, "org.gnome.Shell"}],
    Rmap =
	add_matches(C, MonFs,
		    [
		     "eavesdrop=true,type='method_call',member='ShowOSD'"
		    ], #{}),
    monitor_loop(C,Rmap, 1).

monitor_bluetooth() ->
    monitor([
	     "eavesdrop=true,type='method_call',path=/org/freedesktop/Notifications,member=Notify"
	    ]).

monitor_music() ->
    monitor([
	     "eavesdrop=true,type='signal',path=/org/mpris/MediaPlayer2,member=PropertiesChanged"
	    ]).

pulse_address() -> dbus_pulse:address().
dump_pulse()    -> dbus_pulse:i().
monitor_pulse() -> dbus_pulse:monitor().

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
    MonFs = [{path, "/org/freedesktop/DBus"},
	     {destination,"org.freedesktop.DBus"}],
    monitor(Rules, session, MonFs, _Hello=true, external).

monitor(Rules, Address, MonFs, Hello, AuthType) when is_boolean(Hello) ->
    {ok,C} = dbus_connection:open(Address, AuthType, Hello),
    Rmap = add_matches(C, MonFs, Rules, #{}),
    monitor_loop(C, Rmap, 1).

add_matches(C, MonFs, [Rule|Rules], Acc) ->
    {ok,Ref} = add_match(C, MonFs, Rule),
    add_matches(C, MonFs, Rules, Acc#{ Ref => true });
add_matches(_C, _MonFs, [], Acc) ->
    Acc.
    
add_match(C, MonFs, Rule) ->
    org_freedesktop_dbus:add_match(C, MonFs, Rule).

monitor_loop(C, Refs,I) ->
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
			       Message
			      ]),
		    monitor_loop(C, Refs,I+1);
		false ->
		    monitor_loop(C, Refs,I)
	    end;
	{dbus_match_stop, Ref} ->
	    case maps:get(Ref,Refs,false) of
		true ->
		    ok;
		false ->
		    monitor_loop(C, Refs,I)
	    end;
	Other ->
	    io:format("GOT OTHER: ~p\n", [Other]),
	    monitor_loop(C, Refs,I)
    end.
