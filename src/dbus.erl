%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Top level dbus constructs
%%% @end
%%% Created :  5 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(dbus).

-compile(export_all).

open() ->
    dbus_connection:open().

open(Address) ->
    dbus_connection:open(Address).


%% standard method
ping(Connection) ->
    dbus_connection:call(Connection,
			 [{destination,"org.freedesktop.DBus"},
			  {path,"/org/freedesktop/DBus"},
			  {interface, "org.freedesktop.DBus.Peer"},
			  {member,"Ping"}],
			 "", []).

get_machine_id(Connection) ->
    dbus_connection:call(Connection,
			 [{destination,"org.freedesktop.DBus"},
			  {path,"/org/freedesktop/DBus"},
			  {interface, "org.freedesktop.DBus.Peer"},
			  {member,"GetMachineId"}],
			 "", []).

get_connection_unix_user(Connection, BusName) ->
    dbus_connection:call(Connection,
			 [{destination,"org.freedesktop.DBus"},
			  {path,"/org/freedesktop/DBus"},
			  {interface, "org.freedesktop.DBus"},
			  {member,"GetConnectionUnixUser"}],
			 "s", [BusName]).

    
