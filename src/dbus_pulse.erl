%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/#controlapi
%%% @end
%%% Created : 14 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_pulse).

-export([address/0, address/1]).
-export([i/0, i/1]).
-export([set_profile/2, set_volume/2]).

-compile(export_all).

-include("../include/dbus.hrl").

-define(INTERFACE, "org.PulseAudio.Core1").
-define(ROOT,      "/org/pulseaudio/core1").
-define(PROP_INTERFACE, "org.freedesktop.DBus.Properties").

-define(IF,     ?INTERFACE).
-define(IFCARD, ?INTERFACE++".Card").
-define(IFPROF, ?INTERFACE++".CardProfile").
-define(IFDEV,  ?INTERFACE++".Device").
-define(IFDEVPORT,  ?INTERFACE++".DevicePort").
-define(CARD(Card), filename:join(?ROOT,(Card))).
-define(CARDPROF(Card,Prof), filename:join([?ROOT,(Card),(Prof)])).
-define(DEV(Dev), filename:join(?ROOT,(Dev))).
-define(DEVPORT(Dev,Port), filename:join([?ROOT,(Dev),(Port)])).

-define(PA_SAMPLE_U8,   0).
-define(PA_SAMPLE_ALAW, 1).
-define(PA_SAMPLE_ULAW, 2).
-define(PA_SAMPLE_S16LE, 3).
-define(PA_SAMPLE_S16BE, 4).
-define(PA_SAMPLE_FLOAT32LE, 5).
-define(PA_SAMPLE_FLOAT32BE, 6).
-define(PA_SAMPLE_S32LE, 7).
-define(PA_SAMPLE_S32BE, 8).
-define(PA_SAMPLE_S24LE, 9).
-define(PA_SAMPLE_S24BE, 10).
-define(PA_SAMPLE_S24_32LE, 11).
-define(PA_SAMPLE_S24_32BE, 12).

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
    org_pulseaudio_core1:listen_for_signal(Connection,Fs,Signal,Objects),
    {ok,Ref}.

stop_listening_for_signal(Connection,Fs,Ref) ->
    case dbus_connection:remove_subscrition(Connection, Ref) of
	{ok,{Interface,Member}} ->
	    %% fixme??? integrate better in dbus_connection server????
	    Signal = Interface++"."++Member,
	    org_pulseaudio_core1:stop_listening_for_signal(Connection,Fs,Signal);
	ok -> %% ok but signal is still used
	    ok;
	{error, enoent} ->  %% already removed
	    ok
    end.

introspect() ->
    {ok, Xml} = dbus:introspect(address(), ?ROOT, ?INTERFACE),
    io:put_chars(Xml).

introspect_card() ->
    introspect_card(card0).
introspect_card(Card) ->
    {ok, Xml} = dbus:introspect(address(), ?CARD(Card), ?IFCARD),
    io:put_chars(Xml).

introspect_card_profile() ->
    introspect_card_profile(card0,profile0).
introspect_card_profile(Card,Prof) ->
    {ok, Xml} = dbus:introspect(address(), ?CARDPROF(Card,Prof), ?IFPROF),
    io:put_chars(Xml).

introspect_device() ->
    introspect_device(sink1).
introspect_device(Dev) ->
    {ok, Xml} = dbus:introspect(address(), ?DEV(Dev), ?IFDEV),
    io:put_chars(Xml).

introspect_device_port() ->
    introspect_device_port(sink1, port1).
introspect_device_port(Dev,Port) ->
    {ok, Xml} = dbus:introspect(address(), ?DEVPORT(Dev,Port), ?IFDEVPORT),
    io:put_chars(Xml).

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
    org_pulseaudio_core1_card:get_profile_by_name(Connection,
						  [{path,Card}],
						  Name).
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
    dump_pulse_properties(Connection, 0),
%%    {ok,Name}    = get_name(Connection),
%%    {ok,Version} = get_version(Connection),    
    {ok,Cards}   = get_cards(Connection),
    {ok,Sinks}   = get_sinks(Connection),
    {ok,Sources} = get_sources(Connection),
    {ok,Streams} = get_playback_streams(Connection),
%%    io:format("name: ~p\n", [Name]),
%%    io:format("version: ~p\n", [Version]),
    io:format("cards: ~p\n", [Cards]),
    io:format("sinks: ~p\n", [Sinks]), 
    io:format("sources: ~p\n", [Sources]), 
    io:format("streams: ~p\n", [Streams]), 
    %% Cards
    lists:foreach(
      fun(Card) ->
	      io:format("[CARD ~p]\n", [Card]),
	      dump_card(Connection, Card, 1)
      end, Cards),

    %% Sinks
    lists:foreach(
      fun(Sink) ->
	      io:format("[SINK ~s]\n", [Sink]),
	      dump_device(Connection, Sink, 1)
      end, Sinks),
    %% Sources
    lists:foreach(
      fun(Source) ->
	      io:format("[SOURCE ~s]\n", [Source]),
	      dump_device(Connection, Source, 1)
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
     {property_list, fun (V) -> V end, fun cprop/1}
    ].

indent(I) ->
    lists:duplicate(2*I, $\s).

dump_card(Connection, Card, I) ->
    lists:foreach(
      fun(Prop) ->
	      case get_property(card,Prop,Connection,Card) of
		  {error,_} -> nope;
		  {Name,Value} ->
		      io:format("~s~s: ~p\n", [indent(I),Name,Value]),
		      if Prop =:= profiles ->
			      lists:foreach(
				fun(Profile) ->
					io:format("~s[PROFILE ~p]\n",
						  [indent(I),Profile]),
					dump_card_profile(Connection,Profile,
							  I+1)
				end, Value);
			 true ->
			      ok
		      end
	      end
      end, card_properties()).

pulse_properties() ->
    [
     name,
     version,
     is_local,
     user_name,
     host_name,
     default_channels,
     {default_sample_format,fun from_alsa_format/1, fun to_alsa_format/1},
     default_sample_rate,
     fallback_source
    ].

dump_pulse_properties(Connection, I) ->
    lists:foreach(
      fun(Prop) ->
	      case get_property(undefined,Prop,Connection,"") of
		  {error,_} -> nope;
		  {Name,Value} ->
		      io:format("~s~s: ~p\n", [indent(I),Name,Value])
	      end
      end, pulse_properties()).
     
profile_properties() ->
    [
     index,
     name,
     description,
     sinks,
     sources,
     priority
    ].

dump_card_profile(Connection, Profile, I) ->
    lists:foreach(
      fun(Prop) ->
	      case get_property(card_profile,Prop,Connection,Profile) of
		  {error,_} -> nope;
		  {Name,Value} ->
		      io:format("~s~s: ~p\n", [indent(I),Name,Value])
	      end
      end, profile_properties()).

device_properties() ->
    [
     name,
     driver,
     owner_module,
     card,
     {sample_format,fun from_alsa_format/1, fun to_alsa_format/1},
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
     {property_list, fun(V) -> V end, fun cprop/1}
    ].

dump_device(Connection, Dev, I) ->
    lists:foreach(
      fun(Prop) ->
	      case get_property(device,Prop,Connection,Dev) of
		  {error,_} -> nope;
		  {Name,Value} ->
		      io:format("~s~s: ~p\n",[indent(I),Name,Value])
	      end
      end, device_properties()).

get_property(Kind, Prop, Connection, Path) ->
    get_prop_(Kind, Prop, Connection, Path).

get_prop_(Kind, Prop, Connection, Path) ->
    {PropName,Decode} = 
	case Prop of
	    {PropNm,_Enc,Dec} -> {PropNm,Dec};
	    PropNm when is_atom(PropNm) -> {PropNm, fun(V) -> V end}
	end,
    {Func,Args} = if Kind =:= undefined ->
			  {list_to_atom("get_"++atom_to_list(PropName)),
			   []};
		     true ->
			  {list_to_atom("get_"++atom_to_list(Kind)++"_"++
					    atom_to_list(PropName)),
			   [Path]}
		  end,
    case apply(dbus_pulse, Func, [Connection | Args]) of
	{ok, Value} -> {PropName,Decode(Value)};
	Error -> Error
    end.

to_alsa_format(Value) ->
    maps:get(Value, 
	     #{ ?PA_SAMPLE_U8 => u8,
		?PA_SAMPLE_ALAW => a_law,
		?PA_SAMPLE_ULAW => mu_law,
		?PA_SAMPLE_S16LE => s16_le,
		?PA_SAMPLE_S16BE => s16_be,
		?PA_SAMPLE_FLOAT32LE => float_le,
		?PA_SAMPLE_FLOAT32BE => float_be,
		?PA_SAMPLE_S32LE => s32_le,
		?PA_SAMPLE_S32BE => s32_be, 
		?PA_SAMPLE_S24LE => s24_3le,
		?PA_SAMPLE_S24BE => s24_3be,
		?PA_SAMPLE_S24_32LE => s24_le,
		?PA_SAMPLE_S24_32BE => s24_be
	      }, Value).

from_alsa_format(Format) ->
    maps:get(Format, 
	     #{ u8 => ?PA_SAMPLE_U8,
		a_law => ?PA_SAMPLE_ALAW,
		mu_law => ?PA_SAMPLE_ULAW,
		s16_le => ?PA_SAMPLE_S16LE,
		s16_be => ?PA_SAMPLE_S16BE,
		float_le => ?PA_SAMPLE_FLOAT32LE,
		float_be => ?PA_SAMPLE_FLOAT32BE,
		s32_le => ?PA_SAMPLE_S32LE,
		s32_be => ?PA_SAMPLE_S32BE, 
		s24_3le => ?PA_SAMPLE_S24LE,
		s24_3be => ?PA_SAMPLE_S24BE,
		s24_le  => ?PA_SAMPLE_S24_32LE,
		s24_be => ?PA_SAMPLE_S24_32BE
	      }).
    

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
		    io:format("[NEW CARD: ~p]\n", [Card]),
		    dump_card(C, Card, 1),
		    {ok,Sinks} = get_card_sinks(C, Card),
		    {ok,Sources} = get_card_sources(C, Card),
		    %% Sinks
		    lists:foreach(
		      fun(Sink) ->
			      io:format("[SINK ~s]\n", [Sink]),
			      dump_device(C, Sink,2)
		      end, Sinks),
		    %% Sources
		    lists:foreach(
		      fun(Source) ->
			      io:format("[SOURCE ~s]\n", [Source]),
			      dump_device(C, Source,2)
		      end, Sources),		    
		    io:format("-------------------\n"),
		    monitor_loop(C,Refs,I);
		{"org.PulseAudio.Core1", "CardRemoved"} ->
		    [Card|_] = Message,
		    io:format("[REMOVE CARD: ~p]\n", [Card]),
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
