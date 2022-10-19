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
%%%    Generate interface modules
%%% @end
%%% Created : 18 Feb 2013 by Tony Rogvall <tony@rogvall.se>

-module(dbus_compile).

-compile(export_all).

-define(is_upper(C), (((C) >= $A) andalso ((C) =< $Z))).
-define(is_lower(C), (((C) >= $a) andalso ((C) =< $z))).

%% convert interface name with dot into a module friendly name
module_name(Cs) ->
    [ if C =:= $. -> $_; true -> C end || C <- string:to_lower(Cs) ].

%% decamel method name
function_name([A|Cs]) ->
    if ?is_upper(A) ->
	    [string:to_lower(A)|function_name_(Cs)];
       true ->
	    function_name_(Cs)
    end.

function_name_([A,B|Cs]) ->
    if ?is_upper(B), not ?is_upper(A) ->
	    [A,$_,string:to_lower(B) | function_name_(Cs)];
       ?is_upper(A) ->
	    [string:to_lower(A)|function_name_([B|Cs])];
       true ->
	    [A|function_name_([B|Cs])]
    end;
function_name_([C]) ->
    [string:to_lower(C)];
function_name_([]) -> [].

arg_name([C|Cs]) ->
    if ?is_lower(C) ->
	    [(C-$a)+$A | arg_name_(Cs)];
       ?is_upper(C) ->
	    [C | arg_name_(Cs)];
       C =:= $- ->
	    [$_ | arg_name_(Cs)]
    end.

arg_name_([$-|Cs]) -> [$_|arg_name_(Cs)];
arg_name_([C|Cs]) ->  [C |arg_name_(Cs)];
arg_name_([]) ->  [].

private_files() ->
    D = code:priv_dir(dbus),
    [{erl,D,F} ||
	F <-
	    ["org.freedesktop.DBus",
	     "org.freedesktop.DBus.Introspectable",
	     "org.freedesktop.DBus.Properties",
	     "org.freedesktop.DBus.Peer",
	     "org.freedesktop.DBus.ObjectManager",
	     %% Erlang hack
	     "org.erlang.DBus"
	    ]] ++
	[{xml,D,F} ||
	    F <-
		[%% media
		 "org.mpris.MediaPlayer2.Player.xml",
		 "io.snapcraft.Launcher.xml"
		]
	].

linux_files() ->
    D = "/usr/share/dbus-1/interfaces",
    {ok,Files} = file:list_dir(D),
    [ {xml,D,F} || F <- Files, filename:extension(F) =:= ".xml" ].

%% remove files that do not go pass xml parser!
buggy_files() ->
    D = "/usr/share/dbus-1/interfaces",
    Files = ["net.reactivated.Fprint.Device.xml",
	     %% remove "all" Fprint xml
	     "net.reactivated.Fprint.Manager.xml"
	    ],
    [ {xml,D,F} || F <- Files ].

builtin() ->
    BuildDir = filename:join(code:lib_dir(dbus), "build"),
    BeamDir = filename:join(code:lib_dir(dbus), "ebin"),
    Files = ((case os:type() of
		 {unix,linux} -> linux_files();
		 _ -> []
	      end) -- buggy_files()) ++ private_files(),
    lists:foreach(
      fun({erl,Src,F}) ->
      	      SrcFile = filename:join(Src, F),
	      io:format("Generate: ~s\n", [SrcFile]),
	      DstFiles = generate_from_erl(SrcFile,BuildDir),
	      compile_files(DstFiles, BeamDir, []);
	 ({xml,Src,F}) ->
      	      SrcFile = filename:join(Src, F),
	      io:format("Generate: ~s\n", [SrcFile]),
	      DstFiles = generate_from_xml(SrcFile,BuildDir),
	      compile_files(DstFiles, BeamDir, [])
      end, Files).
%%
%% Compile a list of erlang file, stop at first error
%%
compile_files([File|Files],BeamDir,Acc) ->
    case compile_file(File,BeamDir) of
	{ok,Mod} -> compile_files(Files,BeamDir,[Mod|Acc]);
	error -> error
    end;
compile_files([],_BeamDir,Acc) ->
    lists:reverse(Acc).

%%
%% Compile one erlang file
%%
compile_file(File,BeamDir) ->
    case compile:file(File, [{outdir,BeamDir},
			     return_errors,
			     return_warnings]) of
	{ok,Module,Warnings} ->
	    [io:format("~s:~w: ~s\n", [F,Line,Mod:format_error(W)]) ||
		{F,[{Line,Mod,W}]} <- Warnings],
	    {ok,Module};
	{error,Errors,Warnings} ->
	    [io:format("~s:~w: ~s\n", [F,Line,Mod:format_error(E)]) ||
		{F,[{Line,Mod,E}]} <- Errors],
	    [io:format("~s:~w: ~s\n", [F,Line,Mod:format_error(W)]) ||
		{F,[{Line,Mod,W}]} <- Warnings],
	    error
    end.

%% generate from xml file
generate_from_xml(In,OutDir) ->
    case dbus_xml:load(In) of
	{ok,[]} ->
	    io:format("Error: ~p\n", [{error,missing_interface}]),
	    [];
	{ok,IFs} ->
	    generate_interfaces(IFs, OutDir, []);
	Error={error,_} ->
	    io:format("Error: ~p\n", [Error]),
	    []
    end.

%% generate from erlang (consult) file
generate_from_erl(In,OutDir) ->
    case file:consult(In) of
	{ok,[]} ->
	    io:format("Error: ~p\n", [{error,missing_interface}]),
	    [];
	{ok,IFs} ->
	    generate_interfaces(IFs, OutDir, []);
	Error={error,_} ->
	    io:format("Error: ~p\n", [Error]),
	    []
    end.

generate_interfaces([{interface,Name,Procs}|IFs], Dir, Acc) ->
    ErlFile = filename:join(Dir, module_name(Name)++".erl"),
    Data = generate_interface(Name, Procs),
    file:write_file(ErlFile, Data),
    generate_interfaces(IFs, Dir, [ErlFile | Acc]);
generate_interfaces([], _Dir, Acc) ->
    lists:reverse(Acc).
%%
%% add object path option ? like {objpath"/org/freedesktop/DBus"}
%%
generate_interface(Interface, Procs) ->
    ["-module(", module_name(Interface), ").\n",
     "-compile(export_all).\n\n",
     lists:foldr(
       fun({method,Method,Args,_Ret},Acc) ->
	       {Signature,ArgList} = generate_args(Args,[],[]),
	       ArgListString = string:join(ArgList,","),
	       ParamsString = if Args =:= [] -> "";
				 true -> [$, | ArgListString]
			      end,
	       FunctionName = function_name(Method),
	       [FunctionName,"(Connection,Fs",ParamsString,") -> \n",
		"    dbus_connection:call(Connection,\n",
		"        [{interface,\"",Interface,"\"},\n",
		"         {member,\"",Method,"\"}|Fs],\n",
		"    \"", Signature, "\",\n",
		"    [", ArgListString, "]).\n\n" |
		Acc];
	  ({signal,Signal,Args},Acc) ->
	       {Signature,ArgList} = generate_args(Args,[],[]),
	       ArgListString = string:join(ArgList,","),
	       ParamsString = if Args =:= [] -> "";
				 true -> [$, | ArgListString]
			      end,
	       FunctionName = function_name(Signal),
	       [FunctionName,"(Connection,Fs",ParamsString,") -> \n",
		"    dbus_connection:signal(Connection,\n",
		"        [{interface,\"",Interface,"\"},\n",
		"         {member,\"",Signal,"\"}|Fs],\n",
		"    \"", Signature, "\",\n",
		"    [", ArgListString, "]).\n\n" |
		Acc];
	  (_,Acc) ->
	       Acc
       end,[],Procs)].

generate_args([{Type,Name}|Ts], SigList, ArgList) ->
    Sig = case Type of
	      {enum,IntType,_Enums} ->
		  dbus_codec:type_spec_to_signature(IntType);
	      {bits,IntType,_Bits} ->
		  dbus_codec:type_spec_to_signature(IntType);
	      _ ->
		  dbus_codec:type_spec_to_signature(Type)
	  end,
    Arg = case Name =:= undefined of 
        true ->
            "unnamed";
        false ->
            arg_name(Name)
    end,
    generate_args(Ts, [Sig|SigList], [Arg|ArgList]);

generate_args([], SigList, ArgList) ->
    {lists:append(lists:reverse(SigList)),
     lists:reverse(ArgList)}.
