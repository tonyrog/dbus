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

pulse_address() ->
    {ok, C} = dbus_connection:open(session),
    Interface_name = "org.PulseAudio.ServerLookup1",
    Fs = [{destination,"org.PulseAudio1"},
	  {path,"/org/pulseaudio/server_lookup1"}],
    {ok,[{"s",Addr}]} = org_freedesktop_dbus_properties:get(C,Fs,
							  Interface_name,
							  "Address"),
    Addr.


dump_pulse() ->
    PulseAddress = pulse_address(),
    io:format("connect to pulse @ ~s\n", [PulseAddress]),
    {ok,Connection} = dbus_connection:open(PulseAddress, external, false),
    %%Fs = [{path, "/org/pulseaudio/core1"},{destination, "org.PulseAudio1"}],
    {ok,Name} = dbus_pulse:get_name(Connection),
    {ok,Version} = dbus_pulse:get_version(Connection),    
    {ok,Cards} = dbus_pulse:get_cards(Connection),
    {ok,Sinks} = dbus_pulse:get_sinks(Connection),
    {ok,Sources} = dbus_pulse:get_sources(Connection),
    {ok,Streams} = dbus_pulse:get_playback_streams(Connection),
    io:format("Name = ~p\n", [Name]),
    io:format("Version = ~p\n", [Version]),
    io:format("Cards = ~p\n", [Cards]),
    io:format("Sinks = ~p\n", [Sinks]), 
    io:format("Sources = ~p\n", [Sources]), 
    io:format("Streams = ~p\n", [Streams]), 
    %% Cards
    lists:foreach(
      fun(Card) ->
	      io:format("Card ~p\n", [Card]),
	      dump_pulse_card(Connection, Card)
      end, Cards),

    %% Sinks
    lists:foreach(
      fun(Sink) ->
	      io:format("[Sink ~s]\n", [Sink]),
	      dump_pulse_device(Connection, Sink)
      end, Sinks),
    %% Sources
    lists:foreach(
      fun(Source) ->
	      io:format("[Source ~s]\n", [Source]),
	      dump_pulse_device(Connection, Source)
      end, Sources),
    dbus_connection:close(Connection).

set_card_profile(Card, Profile) ->
    PulseAddress = pulse_address(),
    {ok,Connection} = dbus_connection:open(PulseAddress, external, false),
    R = dbus_pulse:set_card_active_profile(Connection, Card, Profile),
    dbus_connection:close(Connection),
    R.

set_device_volume(Device, Volume) ->
    PulseAddress = pulse_address(),
    {ok,Connection} = dbus_connection:open(PulseAddress, external, false),
    R = dbus_pulse:set_device_volume(Connection, Device, Volume),
    dbus_connection:close(Connection),
    R.

card_properties() ->
    [
     index,
     name,
     driver,
     owner_module,
     sinks,
     sources,
     profiles,
     active_profile,
     property_list
    ].

dump_pulse_card(Connection, Card) ->
    lists:foreach(
      fun(Prop) ->
	      Value = get_property(card,Prop,Connection,Card),
	      io:format("~s: ~p\n", [Prop, Value]),
	      if Prop =:= profiles ->
		      lists:foreach(
			fun(Profile) ->
				dump_pulse_card_profile(Connection,Profile)
			end, Value);
		 true ->
		      ok
	      end
      end, card_properties()).

profile_properties() ->
    [
     index,
     name,
     description,
     sinks,
     sources,
     priority
    ].

dump_pulse_card_profile(Connection, Profile) ->
    io:format("Profile ~p\n", [Profile]),
    lists:foreach(
      fun(Prop) ->
	      Value = get_property(card_profile,Prop,Connection,Profile),
	      io:format("  ~s: ~p\n", [Prop, Value])
      end, profile_properties()).
    
device_properties() ->
    [
     name,
     driver,
     owner_module,
     card,
     sample_format,
     sample_rate,
     channels,
     volume,
     has_flat_volume,
     has_convertible_to_decibel_volume,
     base_volume,
     volume_steps,
     mute,
     has_hardware_volume,
     has_hardware_mute,
     configured_latency,
     has_dynamic_latency,
     latency,
     is_hardware_device,
     is_network_device,
     state,
     ports,
     active_port,
     property_list
    ].

dump_pulse_device(Connection, Dev) ->
    lists:foreach(
      fun(Prop) ->
	      Value = get_property(device,Prop,Connection,Dev),
	      io:format("~s: ~p\n",[Prop, Value])
      end, device_properties()).

get_property(Kind,property_list,Connection, Path) ->
    Value = get_prop_(Kind,property_list,Connection,Path),
    cprop(Value);
get_property(Kind, Prop, Connection, Path) ->
    get_prop_(Kind, Prop, Connection, Path).

get_prop_(Kind, Prop, Connection, Path) ->
    Func = list_to_atom("get_"++atom_to_list(Kind)++"_"++atom_to_list(Prop)),
    case apply(dbus_pulse, Func, [Connection, Path]) of
	{ok, Value} ->
	    Value;
	Error ->
	    Error
    end.

monitor_pulse() ->
    monitor_pulse("").

signals() ->
    [
     "org.PulseAudio.Core1.NewCard",    
     "org.PulseAudio.Core1.CardRemoved",
     "org.PulseAudio.Core1.Device.MuteUpdated",
     "org.PulseAudio.Core1.Device.StateUpdated", 
     "org.PulseAudio.Core1.Stream.VolumeUpdated"
     "org.PulseAudio.Core1.Client.ClientEvent",
     "org.PulseAudio.Core1.Client.UpdateProperties",
     "org.PulseAudio.Ext.StreamRestore1.DeviceUpdated",
     "org.PulseAudio.Ext.StreamRestore1.VolumeUpdated",
     "org.PulseAudio.Ext.StreamRestore1.MuteUpdated"
    ].

monitor_pulse(_Filter) ->
    PulseAddress = pulse_address(),
    io:format("connect to pulse @ ~s\n", [PulseAddress]),
    {ok,C} = dbus_connection:open(PulseAddress, external, false),

    Fs = [{path, "/org/pulseaudio/core1"},{destination, "org.PulseAudio1"}],

    Rmap = 
	lists:foldl(
	  fun(Sig, Map) ->
		  %% filter objects (paths) may be given as list
		  {ok,Ref} = dbus_pulse:listen_for_signal(C,Fs,Sig,[]),
		  Map# { Ref => true }
	  end, #{}, signals()),

    monitor_loop(C, Rmap, 1).




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
	{signal, _Ref, Header, Message} ->
	    Fds = Header#dbus_header.fields,
	    case {Fds#dbus_field.interface,Fds#dbus_field.member} of
		{"org.PulseAudio.Core1", "NewCard"} ->
		    [Card|_] = Message,
		    io:format("NEW CARD: ~p\n", [Card]),
		    dump_pulse_card(C, Card),
		    {ok,Sinks} = dbus_pulse:get_card_sinks(C, Card),
		    {ok,Sources} = dbus_pulse:get_card_sources(C, Card),
		    %% Sinks
		    lists:foreach(
		      fun(Sink) ->
			      io:format("[Sink ~s]\n", [Sink]),
			      dump_pulse_device(C, Sink)
		      end, Sinks),
		    %% Sources
		    lists:foreach(
		      fun(Source) ->
			      io:format("[Source ~s]\n", [Source]),
			      dump_pulse_device(C, Source)
		      end, Sources),		    
		    io:format("-------------------\n"),
		    monitor_loop(C,Refs,I);
		{"org.PulseAudio.Core1", "CardRemoved"} ->
		    [Card|_] = Message,
		    io:format("CARD REMOVED: ~p\n", [Card]),
		    io:format("-------------------\n"),
		    monitor_loop(C,Refs,I);
		_ ->
		    dump_signal(I, Header, Message),
		    monitor_loop(C, Refs,I+1)
	    end;
%%		    
%%	{signal, Ref, Header, Message} ->
%%	    case maps:get(Ref, Refs, false) of
%%		true ->
%%		    dump_signal(I, Header, Message),
%%		    monitor_loop(C, Refs,I+1);
%%		false ->
%%		    monitor_loop(C, Refs,I)
%%	    end;
	Other ->
	    io:format("GOT OTHER: ~p\n", [Other]),
	    monitor_loop(C, Refs,I)
    end.

dump_signal(I, Header, Message) ->
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
	      ]).

cprop(Map) when is_map(Map) ->
    maps:map(fun(_K,V) -> cstring(V) end, Map).
		     
%% format property_list values
cstring(Value) when is_binary(Value) ->
    Len = byte_size(Value),
    case Value of
	<<Val:(Len-1)/binary,0>> ->
	    binary_to_list(Val);
	_ ->
	    Value
    end;
cstring(Value) ->
    Value.



    
