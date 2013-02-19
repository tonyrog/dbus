%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2013 by Tony Rogvall <tony@rogvall.se>

-module(dbus_lib).

-import(lists, [map/2, sort/1, reverse/1]).
-compile(export_all).



connect({"launchd:",[{"env",Var}]}) ->
    Path = trim(os:cmd("launchctl getenv "++Var)),
    afunix:connect(Path, connect_options());
connect({"unix:",[{"path",Path}]}) ->
    afunix:connect(Path, connect_options());
connect({"tcp:",[{"family","ipv4"},{"host",Host},{"port",Port}]}) ->
    gen_tcp:connect(Host, list_to_integer(Port), connect_options());
connect({"tcp:",[{"host",Host},{"port",Port}]}) ->
    gen_tcp:connect(Host, list_to_integer(Port), connect_options());
connect([A|As]) ->
    case connect(A) of
	{ok,S} -> {ok,{S,A}};
	_ -> connect(As)
    end;
connect([]) ->
    {error, noaddress}.

connect_options() ->
    [{active,false}, {mode, list}].

%%
%% Parse address list
%%
parse_address(Address) ->
    As = string:tokens(Address, ";"),
    parse_addrlist(As).

parse_addrlist([A|As]) ->
    {Proto,Args} = lists:split(string:chr(A, $:),A),
    KVals         = string:tokens(Args, ","),
    KVs = map(fun(KV) ->
		      {Key,[$=|Val]}=lists:split(string:chr(KV, $=)-1,KV),
		      {Key,Val}
	      end, KVals),
    [{Proto, sort(KVs)} | parse_addrlist(As)];
parse_addrlist([]) ->
    [].


trim(Cs) ->
    reverse(trim_hd(reverse(trim_hd(Cs)))).

trim_hd([$\n|Cs]) -> trim_hd(Cs);
trim_hd([$\r|Cs]) -> trim_hd(Cs);
trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
