%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Manual Pulse/DBUS interface! where is the xml file????
%%%    https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/#controlapi
%%% @end
%%% Created : 14 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_pulse).

-compile(export_all).
-export([address/0, address/1]).
-export([i/0, i/1]).
-export([set_profile/2, set_volume/2]).

-include("../include/dbus.hrl").

-define(INTERFACE, "org.PulseAudio.Core1").
-define(ROOT,      "/org/pulseaudio/core1").
-define(PROP_INTERFACE, "org.freedesktop.DBus.Properties").

-define(IF,     ?INTERFACE).
-define(IFCARD, ?INTERFACE++".Card").
-define(IFPROF, ?INTERFACE++".CardProfile").
-define(IFDEV,  ?INTERFACE++".Device").
-define(IFDEVPORT,  ?INTERFACE++".DevicePort").
-define(CARD(X), filename:join(?ROOT, X)).
-define(DEV(X), filename:join(?ROOT, X)).
-define(DEVPORT(X,Y), filename:join([?ROOT, X, Y])).

address() ->
    address(session).

address(Address) ->
    {ok, C} = dbus_connection:open(Address),
    Addr = address_(C),
    dbus_connection:close(C),
    Addr.

address_(C) ->
    Interface_name = "org.PulseAudio.ServerLookup1",
    Fs = [{destination,"org.PulseAudio1"},
	  {path,"/org/pulseaudio/server_lookup1"}],
    {ok,[{"s",Addr}]} =
	org_freedesktop_dbus_properties:get(C,Fs,Interface_name,
					    "Address"),
    {Addr,external,false}.

%% speical signal handling 
listen_for_signal(Connection,Fs,Signal,Objects) ->
    {ok,Ref} = dbus_connection:subscribe_to_signal(Connection,Signal),
    dbus_connection:call(Connection,
			 [{interface, ?INTERFACE},
			  {member,"ListenForSignal"} | Fs],
			 "sao", [Signal,Objects]),
    {ok,Ref}.

stop_listening_for_signal(Connection,Fs,Ref) ->
    case dbus_connection:remove_subscrition(Connection, Ref) of
	{ok,{Interface,Member}} ->
	    %% fixme??? integrate better in dbus_connection server????
	    dbus_connection:call(Connection,
				 [{interface,?INTERFACE},
				  {member,"StopListenForSignal"} | Fs],
				 "s",
				 [Interface++"."++Member]);
	ok -> %% ok but signal is still used
	    ok;
	{error, enoent} ->  %% already removed
	    ok
    end.

%%%%%%%%%%%%%
%% Top Level
%%%%%%%%%%%%%

%% properties
get_name(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"Name").
get_version(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"Version").
get_is_local(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"IsLocal").
get_user_name(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"Username").
get_host_name(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"Hostname").
get_default_channels(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"DefaultChannels").
set_default_channels(Connection, Channels) ->
    set_prop(Connection,[{path,?ROOT}],"DefaultChannels", {"au", Channels}).
get_default_sample_format(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"DefaultSampleFormat").
set_default_sample_format(Connection, Format) ->
    set_prop(Connection,[{path,?ROOT}],"DefaultSampleFormat", {"u", Format}).
get_default_sample_rate(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"DefaultSampleRate").
set_default_sample_rate(Connection,Rate) -> 
    set_prop(Connection,[{path,?ROOT}],"DefaultSampleRate", {"u", Rate}).
get_cards(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"Cards").
get_sinks(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"Sinks").
get_fallback_sink(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"FallbackSink").
get_sources(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"Sources").
get_fallback_source(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"FallbackSource").
set_fallback_source(Connection, Path) ->
    set_prop(Connection,[{path,?ROOT}],"FallbackSource", {"o", Path}).
get_playback_streams(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"PlaybackStreams").
get_record_streams(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"RecordStreams").
get_samples(Connection) -> 
    get_prop(Connection,[{path,?ROOT}],"Samples").
get_modules(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"Modules").
get_clients(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"Clients").
get_my_client(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"MyClient").
get_extensions(Connection) ->
    get_prop(Connection,[{path,?ROOT}],"Extensions").

%%%%%%%%%%%%%
%% Cards
%%%%%%%%%%%%%

%% Properties
get_card_index(Connection, Card) ->
   get_card_prop(Connection,[{path,Card}],"Index").
get_card_name(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"Name").
get_card_driver(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"Driver").
get_card_owner_module(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"OwnerModule").
get_card_sinks(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"Sinks").
get_card_sources(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"Sources").
get_card_profiles(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"Profiles").
get_card_active_profile(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"ActiveProfile").
set_card_active_profile(Connection, Card, Profile) ->
    set_card_prop(Connection,[{path,Card}],"ActiveProfile", {"o", Profile}).
get_card_property_list(Connection, Card) ->
    get_card_prop(Connection,[{path,Card}],"PropertyList").

%% Methods
get_card_profile_by_name(Connection, Card, Name) ->
    dbus_connection:call(Connection,
			 [{interface, ?INTERFACE},
			  {path, Card},
			  {member,"GetProfileByName"}],
			 "s", [Name]).

%% Properties
get_card_profile_index(Connection, Profile) ->
    get_card_profile_prop(Connection,[{path,Profile}],"Index").
get_card_profile_name(Connection, Profile) ->
    get_card_profile_prop(Connection,[{path,Profile}],"Name").
get_card_profile_description(Connection, Profile) ->
    get_card_profile_prop(Connection,[{path,Profile}],"Description").

get_card_profile_sinks(Connection, Profile) ->
    get_card_profile_prop(Connection,[{path,Profile}],"Sinks").
get_card_profile_sources(Connection, Profile) ->
    get_card_profile_prop(Connection,[{path,Profile}],"Sources").
get_card_profile_priority(Connection, Profile) ->
    get_card_profile_prop(Connection,[{path,Profile}],"Priority").

%%%%%%%%%%%%%
%% Devices (sinks/sources) dev=sinkX or sourceX
%%%%%%%%%%%%%

get_device_index(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Index").
get_device_name(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Name").
get_device_driver(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Driver").
get_device_owner_module(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"OwnerModule").
get_device_card(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Card").
get_device_sample_format(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"SampleFormat").
get_device_sample_rate(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"SampleRate").
get_device_channels(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Channels").
get_device_volume(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Volume").
set_device_volume(Connection, Dev, Volume) ->
    set_device_prop(Connection,[{path,Dev}],"Volume", {"u", Volume}).
get_device_has_flat_volume(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"HasFlatVolume").
get_device_has_convertible_to_decibel_volume(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"HasConvertibleToDecibelVolume").
get_device_base_volume(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"BaseVolume").
get_device_volume_steps(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"VolumeSteps").
get_device_mute(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Mute").
set_device_mute(Connection, Dev, Mute) ->
    set_device_prop(Connection,[{path,Dev}],"Mute", {"b", Mute}).
get_device_has_hardware_volume(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"HasHardwareVolume").
get_device_has_hardware_mute(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"HasHardwareMute").
get_device_configured_latency(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"ConfiguredLatency").
get_device_has_dynamic_latency(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"HasDynamicLatency").
get_device_latency(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Latency").
get_device_is_hardware_device(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"IsHardwareDevice").
get_device_is_network_device(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"IsNetworkDevice").
get_device_state(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"State").
get_device_ports(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"Ports").
get_device_active_port(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"ActivePort").
set_device_active_port(Connection, Dev, Port) ->
    set_device_prop(Connection,[{path,Dev}],"ActivePort", {"u", Port}).
get_device_property_list(Connection, Dev) ->
    get_device_prop(Connection,[{path,Dev}],"PropertyList").

%% Device ports

get_device_port_index(Connection, DevPort) ->
    get_device_port_prop(Connection,[{path,DevPort}],"Index").
get_device_port_name(Connection, DevPort) ->
    get_device_port_prop(Connection,[{path,DevPort}],"Name").
get_device_port_description(Connection, DevPort) ->
    get_device_port_prop(Connection,[{path,DevPort}],"Description").
get_device_port_priority(Connection, DevPort) ->
    get_device_port_prop(Connection,[{path,DevPort}],"Priority").

%% wrappers for get properties
get_card_prop(Connection,Fs,Name) ->
    get_prop(Connection,Fs,?IFCARD,Name).
set_card_prop(Connection,Fs,Name,Value) ->
    set_prop(Connection,Fs,?IFCARD,Name,Value).

get_card_profile_prop(Connection,Fs,Name) ->
    get_prop(Connection,Fs,?IFPROF,Name).

get_device_prop(Connection,Fs,Name) ->
    get_prop(Connection,Fs,?IFDEV,Name).
set_device_prop(Connection,Fs,Name,Value) ->
    set_prop(Connection,Fs,?IFDEV,Name,Value).

get_device_port_prop(Connection,Fs,Name) ->
    get_prop(Connection,Fs,?IFDEVPORT,Name).

get_prop(Connection,Fs,Name) ->
    get_prop(Connection,Fs,?IF,Name).
get_prop(Connection,Fs,Interface,Name) ->
    case org_freedesktop_dbus_properties:get(Connection,Fs,Interface,Name) of
	{ok,[{_Sigmature,Value}]} -> %% multi value return?
	    {ok,Value};
	Other ->
	    Other
    end.

%% note that the value must be variant! {Signature,Value}
set_prop(Connection,Fs,Name,Value) ->
    set_prop(Connection,Fs,?IF,Name,Value).
set_prop(Connection,Fs,Interface,Name,Value) ->
    org_freedesktop_dbus_properties:set(Connection,Fs,Interface,
					Name,Value).

get_all_props(Connection,Fs) ->
    get_all_props(Connection,Fs,?IF).
get_all_props(Connection,Fs,Interface) ->
    org_freedesktop_dbus_properties:get_all(Connection,Fs,Interface).

%% Utils

set_profile(Card, Profile) ->
    PulseAddress = address(),
    {ok,Connection} = dbus_connection:open(PulseAddress),
    R = set_card_active_profile(Connection, Card, Profile),
    dbus_connection:close(Connection),
    R.

set_volume(Device, Volume) ->
    PulseAddress = address(),
    {ok,Connection} = dbus_connection:open(PulseAddress),
    R = set_device_volume(Connection, Device, Volume),
    dbus_connection:close(Connection),
    R.

i() -> i(session).
i(Address) ->
    PulseAddress = address(Address),
    io:format("connect to pulse @ ~p\n", [PulseAddress]),
    {ok,Connection} = dbus_connection:open(PulseAddress),
    %%Fs = [{path, "/org/pulseaudio/core1"},{destination, "org.PulseAudio1"}],
    {ok,Name} = get_name(Connection),
    {ok,Version} = get_version(Connection),    
    {ok,Cards} = get_cards(Connection),
    {ok,Sinks} = get_sinks(Connection),
    {ok,Sources} = get_sources(Connection),
    {ok,Streams} = get_playback_streams(Connection),
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

monitor() ->
    PulseAddress = address(),
    io:format("connect to pulse @ ~p\n", [PulseAddress]),
    {ok,C} = dbus_connection:open(PulseAddress),
    Fs = [{path, "/org/pulseaudio/core1"},{destination, "org.PulseAudio1"}],
    Rmap = 
	lists:foldl(
	  fun(Sig, Map) ->
		  %% filter objects (paths) may be given as list
		  {ok,Ref} = listen_for_signal(C,Fs,Sig,[]),
		  Map# { Ref => true }
	  end, #{}, signals()),
    monitor_loop(C, Rmap, 1).


monitor_loop(C, Refs,I) ->
    receive
	{signal, _Ref, Header, Message} ->
	    Fds = Header#dbus_header.fields,
	    case {Fds#dbus_field.interface,Fds#dbus_field.member} of
		{"org.PulseAudio.Core1", "NewCard"} ->
		    [Card|_] = Message,
		    io:format("NEW CARD: ~p\n", [Card]),
		    dump_pulse_card(C, Card),
		    {ok,Sinks} = get_card_sinks(C, Card),
		    {ok,Sources} = get_card_sources(C, Card),
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
