%%
%%    Control browser
%%
-module(dbus_launcher).

-export([open/1]).

open(Url) ->
    {ok, C} = dbus_connection:open(session),
    Fs = [{path,"/io/snapcraft/Launcher"},
	  {destination, "io.snapcraft.Launcher"}],
    io_snapcraft_launcher:open_url(C, Fs, Url).
