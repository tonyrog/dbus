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
%%%    Convert DBUS xml interfaces desctiptions to erlang format
%%% @end
%%% Created : 15 Feb 2013 by Tony Rogvall <tony@rogvall.se>

-module(dbus_xml).

-compile([load/1]).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-import(lists, [reverse/1]).

load(File) ->
   Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
		  {Acc, P, S};  % new return format
	     (X, Acc, S) ->
		  {[X|Acc], S}
	  end,
    SearchPath = code:priv_dir(dbus),
    case xmerl_scan:file(File, [{space,normalize},
				{validation,off},
				{acc_fun, Acc},
				{fetch_path, [SearchPath]}
			       ]) of
	Error = {error,_} ->
	    Error;
	{Xml,_Rest} ->
	    Node = xmerl_lib:simplify_element(Xml),
	    {ok, to_interfaces(Node)}
    end.


to_interfaces({node,_,IFs}) ->
    get_interfaces(IFs).

get_interfaces([{interface,[{name,Name}|_],Content}|IFs]) ->
    [ {interface, Name, get_elems(Content)} |
      get_interfaces(IFs)];
get_interfaces([]) ->
    [].

get_elems([{method,[{name,Name}|_],MethodBody}|Rest]) ->
    {In,Out} = get_arguments(MethodBody,"in",[],[]),
    [{method,Name,In,Out} | get_elems(Rest)];
get_elems([{signal,[{name,Name}|_],MethodBody}|Rest]) ->
    {[],Out} = get_arguments(MethodBody,"out",[],[]),
    [{signal,Name,Out} | get_elems(Rest)];

get_elems([{property,[{name,_Name}|_],_}|Rest]) ->
    %% FIXME: add this
    get_elems(Rest);
get_elems([{annotation,_,_}|Rest]) ->
    get_elems(Rest);
get_elems([{'doc:doc',_,_}|Rest]) ->
    get_elems(Rest);
get_elems([_Other|Rest]) ->
    io:format("Other element: ~p\n", [_Other]),
    get_elems(Rest);
get_elems([]) ->
    [].

get_arguments([{arg,Attr,_}|As],DDir,I,O) ->
    Name = proplists:get_value(name, Attr),
    Spec = proplists:get_value(type, Attr),
    Type = dbus_codec:signature_to_type_spec(Spec),
    case proplists:get_value(direction,Attr,DDir) of
	"in" ->
	    get_arguments(As,DDir,[{Type,Name}|I],O);
	"out" ->
	    get_arguments(As,DDir,I,[{Type,Name}|O])
    end;
get_arguments([{annotation,_,_}|As],DDir,I,O) ->
    get_arguments(As,DDir,I,O);
get_arguments([{'doc:doc',_,_}|As],DDir,I,O) ->
    get_arguments(As,DDir,I,O);
get_arguments([_A|As],DDir,I,O) ->
    io:format("Other arg: ~p\n", [_A]),
    get_arguments(As,DDir,I,O);
get_arguments([],_DDir,I,O) ->
    {reverse(I), reverse(O)}.





    
     
			
	   
