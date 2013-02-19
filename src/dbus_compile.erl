%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Generate interface modules
%%% @end
%%% Created : 18 Feb 2013 by Tony Rogvall <tony@rogvall.se>

-module(dbus_compile).

-compile(export_all).

-define(is_upper(C), (((C) >= $A) andalso ((C) =< $Z))).

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

    
generate() ->
    Src = code:priv_dir(dbus),
    Dst = code:priv_dir(dbus),
    BeamDir = filename:join(code:lib_dir(dbus), "ebin"),
    Files = ["org.freedesktop.DBus",
	     "org.freedesktop.DBus.Introspectable",
	     "org.freedesktop.DBus.Properties",
	     "org.freedesktop.DBus.Peer",
	     "org.freedesktop.DBus.ObjectManager"],
    lists:foreach(
      fun(F) ->
	      SrcFile = filename:join(Src, F),
	      DstFile = filename:join(Dst, module_name(F)++".erl"),
	      case generate(SrcFile, DstFile) of
		  ok ->
		      case compile:file(DstFile, [{outdir,BeamDir},
						  return_errors,
						  return_warnings]) of
			  {ok,Module,Warnings} ->
			      [io:format("~s\n", [compile:format_error(W)]) ||
				  W <- Warnings],
			      {ok,Module};
			  {error,Errors,Warnings} ->
			      [io:format("~s\n", [compile:format_error(E)]) ||
				  E <- Errors],
			      [io:format("~s\n", [compile:format_error(W)]) ||
				  W <- Warnings],
			      error
		      end;
		  Error ->
		      Error
	      end
      end, Files).

generate(In, Out) ->
    case file:consult(In) of
	{ok,[{interface,Name,Procs}]} ->
	    Data = generate_interface(Name, Procs),
	    file:write_file(Out, Data),
	    ok;
	{ok,[{_,_,_}]} ->
	    {error,bad_interface};
	{ok,[_,_|_]} ->
	    {error,only_one_interface};
	{ok,[]} ->
	    {error,missing_interface};
	Error={error,_} ->
	    Error
    end.


generate_interface(Name, Procs) ->
    Interface = atom_to_list(Name),
    ["-module(", module_name(Interface), ").\n",
     "-compile(export_all).\n\n",
     lists:foldr(
       fun({method,Method,Args,_Ret},Acc) ->
	       Path = "/org/freedesktop/DBus",
	       {Signature,ArgList} = generate_args(Args,[],[]),
	       ArgListString = string:join(ArgList,","),
	       ParamsString = if Args =:= [] -> "";
				 true -> [$, | ArgListString]
			      end,
	       FunctionName = function_name(atom_to_list(Method)),
	       [FunctionName,"(Connection",ParamsString,") -> \n",
		"    dbus_connection:call(Connection,\n",
		"        [{destination,\"",Interface,"\"},\n",
		"         {path,\"",Path,"\"},\n",
		"         {interface,\"",Interface,"\"},\n",
		"         {member,\"",atom_to_list(Method),"\"}],\n",
		"    \"", Signature, "\",\n",
		"    [", ArgListString, "]).\n\n" |
		Acc];
	  ({signal,_Signal,_Args},Acc) ->
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
    [Ah|At] = atom_to_list(Name),
    Arg = [string:to_upper(Ah)|At],
    generate_args(Ts, [Sig|SigList], [Arg|ArgList]);
generate_args([], SigList, ArgList) ->
    {lists:append(lists:reverse(SigList)),
     lists:reverse(ArgList)}.
