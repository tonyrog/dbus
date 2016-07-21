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
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%     DBUS Erlang dispatcher
%%% @end
%%% Created : 18 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(dbus_erlang).

-behaviour(gen_server).

-include("../include/dbus.hrl").

%% API
-export([start_link/2]).
-export([start/1]).
-export([rpc/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([encode/1, encode/3]).
-export([decode/1, decode/3]).
-export([evariant/1]).

-import(lists, [map/2, append/1, reverse/1]).
-define(SERVER, ?MODULE). 

-define(NAME, "org.erlang.DBus").

-record(state, { 
	  name,       %% org.erlang.DBus.<name>
	  connection 
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start(Node) when is_atom(Node) ->
    {ok,Connection} = dbus:open(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Connection,Node], []).
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Connection,Node) when is_pid(Connection), is_atom(Node) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Connection,Node], []).

%% Demo RPC

rpc(Node, Mod, Fun, Args) ->
    {ok,Connection} = dbus:open(),
    Result = 
	org_erlang_dbus:rpc(Connection, 
			    [{destination,?NAME++"."++atom_to_list(Node)}],
			    Mod, Fun, Args),
    dbus:close(Connection),
    Result.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Connection,Node]) ->
    Name = ?NAME ++ "." ++ atom_to_list(Node),
    case org_freedesktop_dbus:request_name(
	   Connection,
	   [{destination,"org.freedesktop.DBus"},
	    {path,"/org/freedesktop/DBus"}], Name, 0) of
	{ok,[1]} ->
	    link(Connection),
	    {ok, #state { name=Name, connection=Connection }};
	{ok,[_Res]} ->
	    {stop, no_erlang_bus};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({call,Connection,H, Msg}, State) ->
    Fds = H#dbus_header.fields,
    io:format("dbus_erlang: CALL H=~p, Member=~s,Msg=~p\n", 
	      [H,Fds#dbus_field.member,Msg]),
    case Fds#dbus_field.member of
	"Call" ->
	    handle_erlang_call(Connection,H,Msg,State);
	"Rpc" ->
	    handle_erlang_rpc(Connection,H,Msg,State);
    "Introspect" ->
        handle_erlang_introspect(Connection,H,Msg,State);
	_ ->
	    F = #dbus_field { destination  = Fds#dbus_field.sender,
			      sender = State#state.name,
			      reply_serial = H#dbus_header.serial },
	    dbus_connection:error(Connection,
				  F,
				  "org.erlang.DBus.Error.BadMember",
				  "No such Interface Member"),
	    {noreply,State}
    end;

handle_info({signal,Connection,H,Msg}, State) ->
    io:format("dbus_erlang: SIGNAL H=~p, Msg=~p\n", [H,Msg]),
    Fds = H#dbus_header.fields,
    case Fds#dbus_field.member of
	"Cast" ->
	    handle_erlang_cast(Connection,H,Msg,State);
	_ ->
	    io:format("BadMember No such interface"),
	    {noreply,State}
    end;

handle_info(_Info, State) ->
    io:format("dbus_erlang: info=~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_erlang_call(Connection,H,[Server,Request], State) ->
    Sender = State#state.name,
    Destination = (H#dbus_header.fields)#dbus_field.sender,
    Serial = H#dbus_header.serial,
    Pid =
	try list_to_existing_atom(Server) of
	    XServer -> erlang:whereis(XServer)
	catch
	    error:_ -> undefined
	end,
    if Pid =:= undefined ->
	    reply_error(Connection,Sender,Destination,Serial,
			"NoServer", "Server is not registered");
       true ->
	    %% FIXME: we can do this "better"? by spliting
	    %% the gener_server:call in two sides and handle
	    %% the returns manually!
	    spawn(
	      fun() ->
		      Reply = gen_server:call(Pid, Request),
		      reply(Connection,Sender,Destination,Serial,
			    [?DBUS_ERLANG],[Reply])
	      end)
    end,
    {noreply, State}.

handle_erlang_cast(_Connection,_H,[Server,Msg],State) ->
    Pid =
	try list_to_existing_atom(Server) of
	    XServer -> erlang:whereis(XServer)
	catch
	    error:_ -> undefined
	end,
    if Pid =:= undefined ->
	    io:format("dbus_erlang: server ~p not registers\n", [Server]),
	    ok;
       true ->
	    gen_server:cast(Pid, Msg)
    end,
    {noreply, State}.


handle_erlang_rpc(Connection,H,[Mod,Fun,Args], State) ->
    Sender = State#state.name,
    Destination = (H#dbus_header.fields)#dbus_field.sender,
    Serial = H#dbus_header.serial,
    try {list_to_existing_atom(Mod),list_to_existing_atom(Fun)} of
	{M,F} ->
	    spawn(fun() ->
			  try apply(M,F,Args) of
			      Reply ->
				  reply(Connection,Sender,Destination,Serial,
					[?DBUS_ERLANG], [Reply])
			  catch
			      error:Reason ->
				  Text = lists:flatten(io_lib:format("~p", 
								     [Reason])),
				  reply_error(Connection,Sender,Destination,
					      Serial,"Crash", Text)
			  end
		  end),
	    {noreply, State}
    catch
	error:_ ->
	    reply_error(Connection,Sender,Destination,Serial,
			"BadRpc", "Module not loaded"),
	    {noreply, State}
    end.    

handle_erlang_introspect(Connection,H,_,State) ->
    Sender = State#state.name,
    Destination = (H#dbus_header.fields)#dbus_field.sender,
    Serial = H#dbus_header.serial,
    Ebin = filename:dirname(code:which(?MODULE)),
    Priv = filename:join(filename:dirname(Ebin), "priv"),
    {ok, File} = file:read_file(filename:join(Priv, "introspect.xml")),
    Content = unicode:characters_to_list(File), 
    reply(Connection,Sender,Destination,Serial,"s", [Content]).

reply(Connection, Sender, Destination, Serial, Signature, Value) ->
    F = #dbus_field { destination  = Destination,
		      sender       = Sender,
		      reply_serial = Serial },
    dbus_connection:return(Connection, F, Signature, Value).

reply_error(Connection, Sender, Destination, Serial, Error, Text) ->
    F = #dbus_field { destination  = Destination,
		      sender       = Sender,
		      reply_serial = Serial },
    dbus_connection:error(Connection, F,
			  "org.erlang.DBus.Error."++Error,
			  Text).


-define(is_unsigned(X,N), ((X) band (bnot ((1 bsl (N))-1)) =:= 0)).
-define(is_signed(X,N), 
	((((X) >= 0) andalso ?is_unsigned((X),N-1)) orelse
	 ?is_unsigned((-(X))-1,N-1))).

%%
%% @doc 
%%   Encode erlang term
%% @end
%%
encode(X) ->
    encode(erlang:system_info(endian),0,X).

encode(E,Y,X) ->
    {Data,_} = dbus_codec:encode(E,Y,[?DBUS_ERLANG],X),
    iolist_to_binary(Data).

decode(Bin) ->
    decode(erlang:system_info(endian),0,Bin).

decode(E,Y,Bin) ->
    {Term,_Y,<<>>} = dbus_codec:decode(E,Y,[?DBUS_ERLANG],Bin),
    Term.

%% generate variant code 
evariant(X) when is_integer(X) ->
    if ?is_unsigned(X,8) -> {[?DBUS_BYTE],X};
       ?is_signed(X, 16) -> {[?DBUS_INT16],X};
       ?is_signed(X, 32) -> {[?DBUS_INT32],X};
       ?is_signed(X, 64) -> {[?DBUS_INT64],X}
    end;
evariant(X) when is_float(X) ->
    {[?DBUS_DOUBLE],X};
evariant(X) when is_boolean(X) ->
    {[?DBUS_BOOLEAN],X};
evariant(X) when is_binary(X) -> 
    {[?DBUS_ARRAY,?DBUS_BYTE],X};
evariant(X) when is_atom(X) ->
    {[?DBUS_STRING],atom_to_list(X)};
evariant(X) when is_list(X) ->
    S = elist(X),
    if S =:= "av" ->
	    {S, map(fun(Xi) -> evariant(Xi) end, X)};
       true ->
	    {S, X}
    end;
evariant(X) when is_tuple(X) ->
    Ts = map(fun(E) -> evariant(E) end, tuple_to_list(X)),
    Sv = append(map(fun({S,_}) -> S end, Ts)),
    Ev = map(fun({_,Xi}) -> Xi end, Ts),
    {"("++Sv++")", list_to_tuple(Ev)}.

%%
%% find the most general list type code
%%
%% [1,2,3]    => ay
%% [1,2,1000] => aq | an
%% [1.0,2.0]  => ad
%% [true,false] => ab
%% not matching => av  (each element must be variant coded)
%%
elist(L) ->
    elist(L, "").

elist(_, "v") -> 
    "av";
elist([H|T], S) ->
    {S1,_} = evariant(H),
    elist(T, unify(S,S1));
elist([],"y") -> "s";
elist([],S) -> "a"++S.

%% y=uint8, n=int16, i=int32, x=int64
unify("", S) -> S;
unify(S, S) -> S;
unify("v",_) -> "v";
unify([$(|As],[$(|Bs]) ->
    case unify_array_elems(As,Bs) of
	false -> "v";
	Es -> "("++append(Es)++")"
    end;
%% unify("y", "y") -> "y";
unify("y", "n") -> "n";
unify("y", "i") -> "i";
unify("y", "x") -> "x";

unify("n", "y") -> "n";
%% unify("n", "n") -> "n";
unify("n", "i") -> "n";
unify("n", "x") -> "x";

unify("i", "y") -> "i";
unify("i", "n") -> "i";
%% unify("i", "i") -> "i";
unify("i", "x") -> "x";

unify("x", "y") -> "x";
unify("x", "n") -> "x";
unify("x", "i") -> "x";
%% unify("x", "x") -> "x";

%% unify("d", "d") -> "d";
%% unify("b", "b") -> "b";
%% unify("s", "s") -> "s";
unify(_, _) -> "v".

unify_array_elems(As,Bs) ->
    unify_array_elems(As,Bs,[]).

unify_array_elems(")",")",Acc) -> reverse(Acc);
unify_array_elems(")",_, _) -> false;
unify_array_elems(_, ")",_) -> false;
unify_array_elems(As,Bs,Acc) ->
    {A,As1} = dbus_codec:next_arg(As),
    {B,Bs1} = dbus_codec:next_arg(Bs),
    C = unify(A,B),
    unify_array_elems(As1,Bs1,[C|Acc]).

