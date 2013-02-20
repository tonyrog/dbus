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

%%
%% open a dbus_connection
%% the connection is authenticated and is hello negotiated automatically.
%% return {ok, Connection} | {error, Reason}
%%
open() ->
    dbus_connection:open().

open(Address) ->
    dbus_connection:open(Address).

%%
%% dbus_compile:generate() is needed !!!
%%

%% standard method peer
ping(C) ->
    org_freedesktop_dbus_peer:ping(C, []).

get_machine_id(C) ->
    org_freedesktop_dbus_peer:get_machineid(C, []).

%%
%% org.freedesktop.DBus 
%%
request_name(C,Name,Flags) -> 
    %% this call has speical treat in dbus_connection. The caller
    %% (owner process) will get calls and signals sent 
    org_freedesktop_dbus:list_names(C, [Name,Flags]).

release_name(C,Name) ->
    %% this call has speical treat in dbus_connection. The name stops
    %% sending to owner process
    org_freedesktop_dbus:list_names(C, [Name]).


list_names(C) ->
    org_freedesktop_dbus:list_names(C, []).

list_activatable_names(C) ->
    org_freedesktop_dbus:list_activatable_names(C, []).

name_has_owner(C,Name) ->
    org_freedesktop_dbus:name_has_owner(C, [Name]).
    

start_servie_by_name(C,Name,Flags) ->
    org_freedesktop_dbus:get_name_owner(C, [Name,Flags]).

update_activation_environment(C,Env) ->
    org_freedesktop_dbus:update_activation_environment(C, [Env]).

get_connection_unix_user(C, BusName) ->
    org_freedesktop_dbus:get_connection_unix_user(C, [BusName]).

get_connection_unix_process_id(C, BusName) ->
    org_freedesktop_dbus:get_connection_unix_process_id(C, [BusName]).

get_name_owner(C,Name) ->
    org_freedesktop_dbus:get_name_owner(C, [Name]).

