# D-bus interface for Erlang

First of all you need to compile

    $ rebar compile

Next you need to generate and compile the D-bus interfaces,
in linux found under /usr/share/dbus-1/interfaces by calling:

    > dbus:setup().

Now you should be able to run a test monitor loop.

    > dbus:monitor().
