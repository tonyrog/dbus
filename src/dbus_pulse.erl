%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Manual Pulse/DBUS interface! where is the xml file????
%%%    https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/#controlapi
%%% @end
%%% Created : 14 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_pulse).

-compile(export_all).

-define(INTERFACE, "org.PulseAudio.Core1").
-define(ROOT,      "/org/pulseaudio/core1").
-define(PROP_INTERFACE, "org.freedesktop.DBus.Properties").

-define(IF,     ?INTERFACE).
-define(IFCARD, ?INTERFACE++".Card").
-define(IFDEV,  ?INTERFACE++".Device").
-define(IFDEVPORT,  ?INTERFACE++".DevicePort").
-define(CARD(X), filename:join(?ROOT, X)).
-define(DEV(X), filename:join(?ROOT, X)).
-define(DEVPORT(X,Y), filename:join([?ROOT, X, Y])).

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
