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
