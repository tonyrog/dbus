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

-include("../include/dbus.hrl").


connect({"launchd:",[{"env",Var}]}) ->
    Path = trim(os:cmd("launchctl getenv "++Var)),
    afunix:connect(Path, connect_options());
connect({"unix:",[{"path",Path}]}) ->
    afunix:connect(Path, connect_options());
connect({"unix:",[{"abstract",Path},{"guid",_GUID}]}) ->
    afunix:connect([0,Path], connect_options());
connect({"unix:",[{"abstract",Path}]}) ->
    afunix:connect([0,Path], connect_options());
connect({"tcp:",[{"family","ipv4"},{"host",Host},{"port",Port}]}) ->
    gen_tcp:connect(Host, list_to_integer(Port), connect_options());
connect({"tcp:",[{"host",Host},{"port",Port}]}) ->
    gen_tcp:connect(Host, list_to_integer(Port), connect_options());
connect([A|As]) ->
    io:format("try connect ~p\n", [A]),
    case connect(A) of
	{ok,S} -> 
	    {ok,{S,A}};
	Error ->
	    io:format("connect error: ~p\n", [Error]),
	    connect(As)
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

parse_match(String) ->
    lists:foldl(
      fun
	  (["type","signal"],Rule) ->
	      Rule#dbus_rule { type=signal };
	  (["type","method_call"],Rule) ->
	      Rule#dbus_rule { type=method_call };
	  (["type","method_return"],Rule) ->
	      Rule#dbus_rule { type=method_return };
	  (["type","error"],Rule) ->
	      Rule#dbus_rule { type=error };
	  (["sender",Sender],Rule) ->
	      Rule#dbus_rule { sender=Sender };
	  (["interface",Interface],Rule) ->
	      Rule#dbus_rule { interface=Interface };
	  (["member",Member],Rule) ->
	      Rule#dbus_rule { member=Member };
	  (["path",Path],Rule) ->
	      Rule#dbus_rule { path=Path };
	  (["path_namespace",Path],Rule) ->
	      Rule#dbus_rule { path_namespace=Path };
	  (["destination",Destination],Rule) ->
	      Rule#dbus_rule { destination=Destination };
	  ([[$a,$r,$g,I],Arg],Rule) when I >= $0, I =< $9 ->
	      Args = [{I-$0, Arg}|Rule#dbus_rule.args],
	      Rule#dbus_rule { args=Args };
	  ([[$a,$r,$g,I,J],Arg],Rule) when I < $6, J >= $0, J =< $9 ->
	      Args = [{(I-$0)*10+(J-$0), Arg}|Rule#dbus_rule.args],
	      Rule#dbus_rule { args=Args };
	  ([[$a,$r,$g,$6,J],Arg],Rule) when J >= $0, J =< $3 ->
	      Args = [{60+(J-$0), Arg}|Rule#dbus_rule.args],
	      Rule#dbus_rule { args=Args };
	  ([[$a,$r,$g,I,$p,$a,$t,$h],Arg],Rule)
	    when I >= $0, I =< $9 ->
	      Args = [{I-$0, Arg}|Rule#dbus_rule.argspath],
	      Rule#dbus_rule { argspath=Args };
	  ([[$a,$r,$g,I,J,$p,$a,$t,$h],Arg],Rule) 
	    when I < $6, J >= $0, J =< $9 ->
	      Args = [{(I-$0)*10+(J-$0), Arg}|Rule#dbus_rule.argspath],
	      Rule#dbus_rule { argspath=Args };
	  ([[$a,$r,$g,$6,J,$p,$a,$t,$h],Arg],Rule) when J >= $0, J =< $3 ->
	      Args = [{60+(J-$0), Arg}|Rule#dbus_rule.argspath],
	      Rule#dbus_rule { argspath=Args };
	  (["arg0namespace",Ns],Rule) ->
	      Rule#dbus_rule { arg0namespace=Ns };
	  (["eavesdrop", "true"], Rule) ->
	      Rule#dbus_rule { eavesdrop=true};
	  (["eavesdrop", "false"], Rule) ->
	      Rule#dbus_rule { eavesdrop=false}
      end,
      #dbus_rule{},
      [ begin [Key,Value] = split(Item, "="),
	      [Key,trim_string(Value)]
	end || Item <- string:tokens(String, ",")]).

match_rule(R, H, M) ->
    F = H#dbus_header.fields,
    ((R#dbus_rule.type =:= undefined) orelse
     (R#dbus_rule.type =:= H#dbus_header.message_type)) andalso
	match_string(R#dbus_rule.sender,F#dbus_field.sender) andalso
	match_string(R#dbus_rule.interface,F#dbus_field.interface) andalso
	match_string(R#dbus_rule.member,F#dbus_field.member) andalso
	match_string(R#dbus_rule.path,F#dbus_field.path) andalso
	match_pathpat(R#dbus_rule.path_namespace,F#dbus_field.path) andalso
	match_string(R#dbus_rule.destination,F#dbus_field.destination) andalso
	match_args(R#dbus_rule.args, F#dbus_field.signature, M) andalso
	match_args_path(R#dbus_rule.argspath, F#dbus_field.signature, M).

%% exact match optional string
match_string(undefined, _) -> true;
match_string(Arg, Arg) -> true;
match_string(_, _) ->  false.

match_pathpat(undefined, _) -> true;
match_pathpat(Pat,Pat) -> true;
match_pathpat(Pat,Arg) -> lists:prefix(Pat,Arg).

%% exact match arguments
match_args(undefined,_, _) -> true;
match_args(Pat, Sig, Args)  when is_list(Args) ->
    match_args_(Pat, signature_to_tuple(Sig), list_to_tuple(Args)).

match_args_([{I,Arg}|As],Sig,Args) ->
    L = size(Args),
    if I < L ->
	    case element(I,Sig) of
		[?DBUS_STRING] -> true;
		[?DBUS_OBJPATH] -> true;
		_ -> false
	    end andalso 
		  (element(I+1,Args) =:= Arg) andalso
		match_args_(As,Sig,Args);
       true ->
	    false
    end;
match_args_([],_Sig, _Args) ->
    true.

match_path(Pat,Arg) ->
    case lists:last(Arg) of
	$/ -> lists:prefix(Arg, Pat);
	_ -> false
    end orelse
    case lists:last(Pat) of
	$/ -> lists:prefix(Pat,Arg);
	_ -> false
    end.

%% exact match arguments
match_args_path(undefined,_, _) -> true;
match_args_path(Pat, Sig, Args) when is_list(Args) ->
    match_args_path_(Pat, signature_to_tuple(Sig), list_to_tuple(Args)).

match_args_path_([{I,Arg}|As],Signature,Args) ->
    L = size(Args),
    if I < L ->
	    case element(I,Signature) of
		[?DBUS_STRING] -> true;
		[?DBUS_OBJPATH] -> true;
		_ -> false
	    end andalso 
		match_path(Arg, element(I+1,Args)) andalso
		match_args_path_(As,Signature,Args);
       true ->
	    false
    end;
match_args_path_([],_Signature, _Args) ->
    true.

signature_to_tuple(Es) ->
    list_to_tuple(signature_to_list(Es, [])).

signature_to_list([],Acc) ->
    lists:reverse(Acc);
signature_to_list(Es,Acc) ->
    {Spec,Es1} = dbus_codec:next_arg(Es),
    signature_to_list(Es1, [Spec|Acc]).

split(String, Token) ->
    case string:str(String, Token) of
	0 -> [String];
	I -> 
	    {Key,Value} = lists:split(I-1,String),
	    [Key,Value--Token]
    end.

trim_string([$'|Cs]) ->    
    case lists:reverse(Cs) of
	[$'|Cs1] -> lists:reverse(Cs1);
	Cs1 -> Cs1
    end;
trim_string([$"|Cs]) ->    
    case lists:reverse(Cs) of
	[$"|Cs1] -> lists:reverse(Cs1);
	Cs1 -> Cs1
    end;
trim_string(Cs) ->
    Cs.

trim(Cs) ->
    reverse(trim_hd(reverse(trim_hd(Cs)))).

trim_hd([$\n|Cs]) -> trim_hd(Cs);
trim_hd([$\r|Cs]) -> trim_hd(Cs);
trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
