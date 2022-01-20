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
%%%
%%% @end
%%% Created : 15 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(dbus_connection).

-behaviour(gen_server).

%% API
-export([open/0, open/1]).
-export([call/4, signal/4, return/4, error/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(lists, [reverse/1]).

-compile(export_all).

%% -define(debug(F,A), io:format((F),(A))).
-define(debug(F,A), ok).
-define(warning(F,A), io:format((F),(A))).


%% status:
%%   undefined
%%   connected,
%%   auth_sent,
%%   authenticated
%%   hello_sent
%%   running
-record(state, {
	  status :: undefined | connected | auth_sent | authenticated |
		    hello_sent | running,
	  socket,             %% current socket
	  addr,               %% used peer address
	  addr_list = [],     %% list of addresses to test
	  guid,
	  endian = little,
	  wait_list = [],     %% [{Serial,From,Callback,Args}] 
	  own_list  = [],     %% [{BusName, OwnerPid}]
	  match_list = [],    %% [{Rule,#dbus_rule{} Rule,Ref,Pid}] 
	  connection_name,    %% return value from hello
	  buf = <<>>,         %% session data buffer
	  client_serial = 1,
	  server_serial = 1
	 }).

-include("../include/dbus.hrl").



%%%===================================================================
%%% API
%%%===================================================================

call(Connection, Fields, Signature, Args) ->
    gen_server:call(Connection, {call,Fields,Signature,Args}).

signal(Connection, Fields, Signature, Args) ->
    gen_server:call(Connection, {signal,Fields,Signature,Args}).

return(Connection, Fields, Signature, Value) ->
    gen_server:call(Connection, {return,Fields,Signature,Value}).

return(Connection, Fields, Data) when is_binary(Data) ->
    gen_server:call(Connection, {return,Fields,Data}).

error(Connection, Fields, ErrorName, ErroText) ->
    gen_server:call(Connection, {error,Fields,ErrorName,ErroText}).

%%--------------------------------------------------------------------
%% @doc
%% Open the connection
%%
%% @spec open(Address::string()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
open() ->
    open(session).

open(system) ->
    case os:type() of
	{unix, darwin} ->
	    open("launchd:env=DBUS_LAUNCHD_SYSTEM_BUS_SOCKET");
	{unix, linux} ->
	    open("unix:path=/var/run/dbus/system_bus_socket")
    end;
open(session) ->
    case os:type() of
	{unix, darwin} ->
	    open("launchd:env=DBUS_LAUNCHD_SESSION_BUS_SOCKET");
	{unix, linux} ->
	    Addr = os:getenv("DBUS_SESSION_BUS_ADDRESS"),
	    open(Addr)
    end;
open(Address) ->
    {ok, C} = gen_server:start_link(?MODULE, [], []),
    ok = set_address(C, Address),
    ok = connect(C),
    ok = authenticate(C, external),
    {ok,[_ConnectionName]} = hello(C),
    {ok, C}.

set_address(C, Address) ->
    As = dbus_lib:parse_address(Address),
    set_addr_list(C, As).

set_addr_list(C, AddrList) ->
    gen_server:call(C, {set_addr_list, AddrList}).

connect(C) ->
    gen_server:call(C, connect).

authenticate(C, Type) ->
    gen_server:call(C, {authenticate,Type}).

hello(C) ->
    gen_server:call(C, hello).

get_is_connected(C) ->
    gen_server:call(C, get_is_connected).

get_is_authenticated(C) ->
    gen_server:call(C, get_is_authenticated).

get_connection_name(C) ->
    gen_server:call(C, get_connection_name).

%% get_is_anoymous
%% get_is_server_id
%% can_send_type


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
init([]) ->
    {ok, #state {}}.

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
handle_call({call,Fields,Signature,Args}, From, State) ->
    ?debug("call fields=~p, signature=~s args=~p\n",
	   [Fields, Signature, Args]),
    case State#state.status of
	running ->
	    Endian = State#state.endian,
	    Serial = State#state.client_serial,
	    Fd     = dbus_message:fields(Fields),
	    {Msg,_L} = dbus_message:encode_call(Endian,Serial,Fd,
						Signature,Args),
	    _Res = send(State#state.socket, Msg),
	    Callback = 
		if Fd#dbus_field.member =:= "RequestName",
		   Fd#dbus_field.interface =:= "org.freedesktop.DBus",
		   Fd#dbus_field.destination =:= "org.freedesktop.DBus" ->
			fun callback_request_name/4;
		   Fd#dbus_field.member =:= "ReleaseName",
		   Fd#dbus_field.interface =:= "org.freedesktop.DBus",
		   Fd#dbus_field.destination =:= "org.freedesktop.DBus" ->
			fun callback_release_name/4;
		   Fd#dbus_field.member =:= "AddMatch",
		   Fd#dbus_field.interface =:= "org.freedesktop.DBus",
		   Fd#dbus_field.destination =:= "org.freedesktop.DBus" ->
			fun callback_add_match/4;
		   Fd#dbus_field.member =:= "RemoveMatch",
		   Fd#dbus_field.interface =:= "org.freedesktop.DBus",
		   Fd#dbus_field.destination =:= "org.freedesktop.DBus" ->
			fun callback_remove_match/4;
		   true ->
			fun callback_return/4
		end,
	    Wait = [{Serial,From,Callback,Args} |
		    State#state.wait_list],
	    {noreply, State#state { client_serial = Serial + 1,
				    wait_list = Wait 
				  }};
	undefined ->
	    {reply, {error,enotcon}, State};
	authenticated ->
	    {reply, {error,need_hello}, State};
	_ ->
	    {reply, {error,eneedauth}, State}
    end;

%% must include path, interface, member
handle_call({signal,Fields,Signature,Args}, _From, State) ->
    ?debug("signal fields=~p, signature=~s args=~p\n",
	   [Fields, Signature, Args]),
    case State#state.status of
	running ->
	    Endian = State#state.endian,
	    Serial = State#state.client_serial,
	    {Msg,_L} = dbus_message:encode_signal(Endian,Serial,
						  Fields,Signature,Args),
	    Res = send(State#state.socket, Msg),
	    {reply, Res, State#state { client_serial = Serial + 1}};
	undefined ->
	    {reply, {error,enotcon}, State};
	authenticated ->
	    {reply, {error,need_hello}, State};
	_ ->
	    {reply, {error,eneedauth}, State}
    end;
handle_call({return,Fields,Signature,Args}, _From, State) ->
    ?debug("return fields=~p, signature=~s args=~p\n",
	   [Fields, Signature, Args]),
    case State#state.status of
	running ->
	    Endian = State#state.endian,
	    Serial = State#state.client_serial,
	    {Msg,_L} = dbus_message:encode_return(Endian,Serial,
						  Fields,Signature,Args),
	    Res = send(State#state.socket, Msg),
	    {reply, Res, State#state { client_serial = Serial + 1}};
	undefined ->
	    {reply, {error,enotcon}, State};
	authenticated ->
	    {reply, {error,need_hello}, State};
	_ ->
	    {reply, {error,eneedauth}, State}
    end;
handle_call({error,Fields,ErrorName,ErrorText}, _From, State) ->
    ?debug("error fields=~p, error=~s test=~s\n",
	   [Fields, ErrorName,ErrorText]),
    case State#state.status of
	running ->
	    Endian = State#state.endian,
	    Serial = State#state.client_serial,
	    {Msg,_L} = dbus_message:encode_error(Endian,Serial,
						 Fields,ErrorName,ErrorText),
	    Res = send(State#state.socket, Msg),
	    {reply, Res, State#state { client_serial = Serial + 1}};
	undefined ->
	    {reply, {error,enotcon}, State};
	authenticated ->
	    {reply, {error,need_hello}, State};
	_ ->
	    {reply, {error,eneedauth}, State}
    end;

handle_call({set_addr_list, As}, _From, State) ->
    ?debug("address list: ~p\n", [As]),
    {reply, ok, State#state { addr_list = As }};

handle_call(connect, _From, State) ->
    case State#state.status of
	undefined when State#state.addr_list /= [] ->
	    case dbus_lib:connect(State#state.addr_list) of
		{ok, {S, A}} ->
		    {reply, ok, 
		     set_status(connected, 
				State#state{ client_serial = 1,
					     server_serial = 1,
					     addr=A, socket=S })};
		Error ->
		    {reply, Error, State}
	    end;
	undefined when State#state.addr_list =:= [] ->
	    {reply, {error, einval}, State};
	_ ->
	    {reply, {error, ealready}, State}
    end;

handle_call(disconnect, _From, State) ->
    if State#state.socket =:= undefined ->
	    ok;
       true ->
	    gen_tcp:close(State#state.socket)
    end,
    {reply, ok, set_status(undefined, State)};

handle_call({authenticate,Type}, From, State) ->
    case State#state.status of
	connected ->
	    inet:setopts(State#state.socket, [{packet,line},{active,once}]),
	    send(State#state.socket, <<0>>),
	    case Type of
		external ->
		    UID = strip_eol(os:cmd("id -u")),
		    send(State#state.socket,
			 ["AUTH EXTERNAL ", list_to_hexlist(UID), "\r\n"]);
		cookie ->
		    User = os:getenv("USER"),
		    send(State#state.socket,
			 ["AUTH DBUS_COOKIE_SHA1 ",
			  list_to_hexlist(User), "\r\n"]);
		detect ->
		    send(State#state.socket,
			 "AUTH\r\n")
	    end,
	    {noreply, set_status(auth_sent,
				 State#state {
				   wait_list = [{0,From,undefined,[]}] })};
	undefined ->
	    {reply, {error, enotconn}, State};
	_->
	    {reply, {error, ealready}, State}
    end;

handle_call(hello, From, State) ->
    case State#state.status of
	authenticated ->
	    Endian = State#state.endian,
	    Serial = State#state.client_serial,
	    Fd = #dbus_field { destination="org.freedesktop.DBus",
			       path="/org/freedesktop/DBus",
			       interface="org.freedesktop.DBus",
			       member="Hello" },
	    {Msg,_L} = dbus_message:encode_call(Endian,Serial,Fd,"",[]),
	    _Res = send(State#state.socket, Msg),
	    inet:setopts(State#state.socket, [{active,once}]),
	    WaitList = [{Serial,From,fun callback_hello/4,[]} |
			State#state.wait_list],
	    {noreply, set_status(hello_sent,
				 State#state {
				   client_serial = Serial + 1,
				   wait_list = WaitList })};
	_ ->
	    {reply, {error,eneedauth}, State}
    end;
handle_call(get_is_connected, _From, State) ->
    {reply, State#state.status =/= undefined, State};
handle_call(get_is_authenticated, _From, State) ->
    case State#state.status of
	authenticated -> {reply, true, State};
	hello_sent    -> {reply, true, State};
	running       -> {reply, true, State};
	_ -> {reply, false, State}
    end;
handle_call(get_connection_name, _From, State) ->
    {reply, State#state.connection_name, State};
	    
handle_call(stop, _From, State) ->
    {stop, normal, State}.

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
handle_info({tcp, S, Data}, 
	    State = #state{socket=S,status=Status}) when 
      is_binary(Data),
      ( (Status =:= authenticated) orelse
	(Status =:= hello_sent) orelse
	(Status =:= running)) ->
    inet:setopts(State#state.socket, [{active,once}]),
    handle_input(Data, State);
handle_info({tcp, S, _Info = "DATA " ++ Line}, 
	    State = #state{socket=S,status=auth_sent}) ->
    ?debug("~s: handle_info:DATA: ~p\n", [?MODULE, _Info]),
    Data = hex_to_list(strip_eol(Line)),
    [_Context, CookieId, ServerChallenge] = string:tokens(Data, " "),
    ?debug("Data: ~p,~p,~p ~n", [_Context, CookieId, ServerChallenge]),
    case read_cookie(CookieId) of
	error ->
	    {stop, {error, {no_cookie, CookieId}}, State};
	{ok, Cookie} ->
	    Challenge = calc_challenge(),
	    Response = calc_response(ServerChallenge, Challenge, Cookie),
	    inet:setopts(State#state.socket, [{active,once}]),
	    ok = send(S, ["DATA " ++ Response ++ "\r\n"]),
	    {noreply, State}
    end;

handle_info({tcp, S, _Info="OK " ++ Line}, 
	    State = #state{socket=S,status=auth_sent}) ->
    ?debug("~s: handle_info:OK: ~p\n", [?MODULE, _Info]),
    Guid = strip_eol(Line),
    error_logger:info_msg("GUID ~p~n", [Guid]),
    ok = send(S, ["BEGIN\r\n"]),
    ok = inet:setopts(S, [binary, {packet,0}, {active,once}]),
    [{0,From,undefined,[]}] = State#state.wait_list,
    gen_server:reply(From, ok),  %% auth done!
    {noreply, set_status(authenticated,
			 State#state { wait_list = [],
				       guid=Guid })};
handle_info({tcp, S, _Info="REJECTED " ++ Line}, 
	    State = #state{socket=S,status=auth_sent}) ->
    ?debug("~s: handle_info:REJCTED: ~p\n", [?MODULE, _Info]),
    Meths=
	lists:foldl(
	  fun("DBUS_COOKIE_SHA1", Acc) -> [cookie|Acc];
	     ("EXTERNAL", Acc) -> [external|Acc];
	     ("ANONYMOUS",Acc) -> [anonymous|Acc];
	     (Other,Acc) -> [{other,Other} | Acc]
	  end, [], string:tokens(strip_eol(Line), "")),
    gen_tcp:close(S),
    [{0,From,undefined,[]}] = State#state.wait_list,
    gen_server:reply(From,
		     {error,{auth_rejected,reverse(Meths)}}),
    {noreply, set_status(undefined, 
			 State#state { socket=undefined, 
				       wait_list = []
				     })};
handle_info({'DOWN',Mon,process,Pid,_Reason}, State) ->
    case lists:keytake(Mon, 2, State#state.own_list) of
	false ->
	    {noreply, State};
	{value,{_Name,Mon,Pid},OwnList} ->
	    %% RelaseName ?
	    {noreply, State#state { own_list = OwnList }}
    end;
handle_info(_Info, State) ->
    ?warning("~s: handle_info: unexpected ~p\n", [?MODULE, _Info]),
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
terminate(_Reason, State) ->
    if State#state.socket =/= undefined ->
	    gen_tcp:close(State#state.socket);
       true ->
	    ok
    end.

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

calc_challenge() ->
    UnixTime = erlang:system_time(seconds),
    Challenge = list_to_hexlist("Hello " ++ integer_to_list(UnixTime)),
    Challenge.

calc_response(ServerChallenge, Challenge, Cookie) ->
    A1 = ServerChallenge ++ ":" ++ Challenge ++ ":" ++ Cookie,
    Digest = crypto:hash(sha,A1),
    DigestHex = binary_to_hexlist(Digest),
    ?debug("A1: ~p\nn", [A1]),
    Response = list_to_hexlist(Challenge ++ " " ++ DigestHex),
    ?debug("Reponse: ~p\n", [Response]),
    Response.

i2h(I) -> 
    element(I+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}).

h2i(X) ->
    if X >= $0, X =< $9 -> (X-$0);
       X >= $A, X =< $F -> (X-$A)+10;
       X >= $a, X =< $f -> (X-$a)+10
    end.

list_to_hexlist([C|Cs]) ->
    [i2h(C bsr 4), i2h(C band 15) | list_to_hexlist(Cs)];
list_to_hexlist([]) ->
    [].

binary_to_hexlist(Bin) -> [i2h(I) || <<I:4>> <= Bin].

hex_to_list([H,L|Cs]) ->
    [ h2i(H)*16+h2i(L) | hex_to_list(Cs)];
hex_to_list([]) ->
    [].

strip_eol([]) -> [];
strip_eol([$\r,$\n]) -> [];
strip_eol([$\r]) -> [];
strip_eol([$\n]) -> [];
strip_eol([C|Cs]) -> [C | strip_eol(Cs)].

read_cookie(CookieId) ->
    Home = os:getenv("HOME"),
    Name = Home ++ "/.dbus-keyrings/org_freedesktop_general",
    {ok, Fd} = file:open(Name, [read]),
    Result = 
	try read_cookie_(Fd, CookieId) of
	    R -> R
	catch
	    error:_ -> error
	after
	    file:close(Fd)
	end,
    Result.

%%
%% Read authentication cookie from file
%%
read_cookie_(Fd, CookieId) ->
    case io:get_line(Fd, "") of
	eof ->
	    error;
	Line ->
	    case string:tokens(strip_eol(Line)," ") of
		[CookieId, _Time, Cookie] ->
		    {ok, Cookie};
		_ ->
		    read_cookie_(Fd, CookieId)
	    end
    end.

send(S, Data) ->
    ?debug("~s: Send ~p\n", [?MODULE, iolist_to_binary(Data)]),
    gen_tcp:send(S, Data).

handle_input(<<>>,State) ->
    {noreply,State};
handle_input(Data,State) when is_binary(Data) ->
    Data1 = case State#state.buf of
		<<>> -> Data;
		Data0 -> <<Data0/binary,Data/binary>>
	    end,
    try dbus_message:decode_dbus_header(0,Data1) of
	{H,Y,Data2} ->
	    P0 = ?PAD_SIZE(Y,8),
	    Length = H#dbus_header.length,
	    if byte_size(Data2) < Length+P0 ->
		    %% we should maybe optimize this, reading Length bytes
		    %% wait for more data
		    ?debug("handle_input: need more data\n", []),
		    {noreply, State#state { buf = Data1 }};
	       true ->
		    <<?PAD(P0),Body:Length/binary,Data3/binary>> = Data2,
		    Fds = H#dbus_header.fields,
		    Msg = case Fds#dbus_field.signature of
			      undefined -> [];
			      Signature ->
				  try dbus_codec:decode_args(
					H#dbus_header.endian,0,
					Signature,Body) of
				      {Message,_Y1,<<>>} ->
					  Message
				  catch
				      error:_Reason:Stack ->
					  io:format("decode args error: ~p\n",
						    [Stack]),
					  ["error"]
				  end
			  end,
		    State1 = State#state { buf = Data3 },
		    handle_msg(H, Msg, State1)
	    end
    catch
	error:more_data ->
	    ?debug("handle_input: need more data\n", []),
	    {noreply, State#state { buf = Data1 }}
    end.

handle_msg(H, Msg, State) ->
    ?debug("handle_msg: header=~1000p, msg=~1000p\n", [H, Msg]),
    handle_match_list(H, Msg, State),
    case H#dbus_header.message_type of
	method_return ->
	    Fds = H#dbus_header.fields,
	    Serial = Fds#dbus_field.reply_serial,
	    case lists:keytake(Serial,1,State#state.wait_list) of
		{value,{_,From,Callback,As},WaitList} ->
		    State1 = Callback({ok,Msg},As,From,State),
		    State2 = State1#state { buf = <<>>,
					    wait_list = WaitList },
		    handle_input(State#state.buf, State2);
		false ->
		    %% warning: bad serial
		    State1 = State#state { buf = <<>> },
		    handle_input(State#state.buf, State1)
	    end;

	error ->
	    Fds = H#dbus_header.fields,
	    Serial = Fds#dbus_field.reply_serial,
	    Error  = Fds#dbus_field.error_name,
	    [ErrorText] = Msg,
	    case lists:keytake(Serial,1,State#state.wait_list) of
		{value,{_,From,Callback,As},WaitList} ->
		    State1 = Callback({error,{Error,ErrorText}},As,From,State),
		    State2 = State1#state { buf = <<>>, wait_list = WaitList },
		    handle_input(State#state.buf, State2);
		false ->
		    State1 = State#state { buf = <<>> },
		    handle_input(State#state.buf, State1)
	    end;

	signal ->
	    Fds = H#dbus_header.fields,
	    Dest = Fds#dbus_field.destination,
	    case lists:keyfind(Dest, 1, State#state.own_list) of
		false ->
		    State1 = State#state { buf = <<>> },
		    handle_input(State#state.buf, State1);
		{_Name,_Mon, Owner} ->
		    Owner ! {signal,self(),H,Msg},
		    State1 = State#state { buf = <<>> },
		    handle_input(State#state.buf, State1)
	    end;

	method_call ->
	    Fds = H#dbus_header.fields,
	    Dest = Fds#dbus_field.destination,
	    case lists:keyfind(Dest, 1, State#state.own_list) of
		false ->
		    State1 = State#state { buf = <<>> },
		    handle_input(State#state.buf, State1);
		{_Name,_Mon,Owner} ->
		    Owner ! {call,self(),H,Msg},
		    State1 = State#state { buf = <<>> },
		    handle_input(State#state.buf, State1)
	    end
    end.

handle_match_list(H, Msg, State) ->
    lists:foreach(
      fun({_Rule,DRule,Ref,Pid}) ->
	      case dbus_lib:match_rule(DRule,H,Msg) of
		  true ->
		      Pid ! {dbus_match,Ref,H,Msg};
		  false ->
		      ignore
	      end
      end, State#state.match_list).

callback_return(Value,_Args, From, State) ->
    gen_server:reply(From, Value),
    State.
%%
%% Handle result of RequestName
%%
callback_request_name({ok,[Code]},[Name,_Flags],From,State) ->
    gen_server:reply(From, {ok,[Code]}),
    case Code of
	1 ->
	    {Pid,_Mref} = From,  %% inital caller
	    Mon = erlang:monitor(process, Pid),
	    OwnList = [{Name,Mon,Pid}|State#state.own_list],
	    State#state { own_list = OwnList };
	_ ->
	    State
    end;
callback_request_name(Value,_Args,From,State) ->
    gen_server:reply(From, Value),
    State.

%%
%% Fixme
%%
callback_release_name({ok,[Code]},[Name], From, State) ->
    gen_server:reply(From, {ok,[Code]}),
    case Code of
	1 ->
	    case lists:keytake(Name, 1, State#state.own_list) of
		false ->
		    State;
		{value,{_,Mon,_Pid},OwnList} ->
		    erlang:demonitor(Mon, [flush]),
		    State#state { own_list = OwnList }
	    end;
	_ ->
	    State
    end;
callback_release_name(Value, [_Name], From, State) ->
    gen_server:reply(From, Value),
    State.


%% Handle result of AddMatch
callback_add_match({ok,[]},[Rule],From,State) ->
    {Pid,_Mref} = From,  %% inital caller
    Mon = erlang:monitor(process, Pid),
    gen_server:reply(From, {ok,Mon}),
    DRule = dbus_lib:parse_match(Rule),
    MatchList = [{Rule,DRule,Mon,Pid}|State#state.match_list],
    State#state { match_list = MatchList };
callback_add_match(Value={error,_},_Args,From,State) ->
    gen_server:reply(From, Value),
    State.


%% Handle result of RemoveMatch
callback_remove_match({ok,[]},[Rule],From,State) ->
    case lists:keytake(Rule,1,State#state.match_list) of
	{value,{Rule,_DRule,Mon,_Pid},MatchList} ->
	    gen_server:reply(From, ok),
	    erlang:demonitor(Mon, [flush]),
	    State#state { match_list = MatchList };
	_ ->
	    gen_server:reply(From, {error,not_found}),
	    State
    end;
callback_remove_match(Value={error,_},_Args,From,State) ->
    gen_server:reply(From, Value),
    State.


%%
%% Handle result of Hello
%%
callback_hello(Value={ok,Msg},_Args,From,State) ->
    gen_server:reply(From, Value),
    set_status(running, State#state { connection_name = hd(Msg) });
callback_hello(Value={error,_},_Args,From,State) ->
    gen_server:reply(From, Value),
    set_status(authenticated, State).

set_status(NewStatus, State) ->
    ?debug("set_status: ~w => ~w\n", [State#state.status, NewStatus]),
    State#state { status = NewStatus }.
