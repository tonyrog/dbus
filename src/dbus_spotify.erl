%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Control spotify / media player
%%% @end
%%% Created : 19 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(dbus_spotify).

-compile(export_all).

open(Uri) ->
    {ok, C} = dbus_connection:open(session),
    Fs = [{path,"/org/mpris/MediaPlay2"},
	  {destination, "org.mpris.MediaPlayer2.spotify"}],
    org_mpris_mediaplayer2_player:open_uri(C, Fs, Uri).

open_url(Url) ->
    {ok, C} = dbus_connection:open(session),
    Fs = [{path,"/io/snapcraft/Launcher"},
	  {destination, "io.snapcraft.Launcher"}],
    io_snapcraft_launcher:open_url(C, Fs, Url).
