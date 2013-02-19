%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     DBus type codec
%%%     Ref: http://dbus.freedesktop.org/doc/dbus-specification.html
%%% @end
%%% Created :  3 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(dbus_codec).

-compile(export_all).

-include("../include/dbus.hrl").

-define(is_unsigned(X,N), ((X) band (bnot ((1 bsl (N))-1)) =:= 0)).
-define(is_signed(X,N), 
	((((X) >= 0) andalso ?is_unsigned((X),N-1)) orelse
	 ?is_unsigned((-(X))-1,N-1))).

-define(ENCODE_UINT(E,X,Sz),    <<(X):Sz/E-unsigned-integer>>).
-define(ENCODE_INT(E,X,Sz),	<<(X):Sz/E-signed-integer>>).
-define(ENCODE_FLOAT(E,X,Sz),	<<(X):Sz/E-float>>).

-define(PAD_BINARY(P,T),
	if (P) =:= 0 -> T;
	   true -> [<<?PAD(P)>>,T]
	end).

-define(PAD_LIST(P,T),
	if (P) =:= 0 -> T;
	   true -> [<<?PAD(P)>>|T]
	end).

-define(MAX_STRING_SIZE,    16#ffffffff).
-define(MAX_OBJPATH_SIZE,   16#ffffffff).
-define(MAX_SIGNATURE_SIZE, 16#ff).
-define(MAX_ARRAY_SIZE,     16#3ffffff).

%% check if Bin contains enough data
-define(NEED_SIZE(Bin, N),
	if byte_size((Bin)) < (N) ->
		erlang:error(more_data);
	   true ->
		true
	end).

-compile({inline,[pad_size/2]}).

pad_size(Y,N) ->
    case (Y band ((N)-1)) of
	0 -> 0;
	R -> N-R
    end.
%%    case N-(Y band ((N)-1)) of
%%	N -> 0;
%%	R -> R
%%    end.

size(?DBUS_BYTE)    -> 1;
size(?DBUS_BOOLEAN) -> 4;
size(?DBUS_INT16)   -> 2;
size(?DBUS_UINT16)  -> 2;
size(?DBUS_INT32)   -> 4;
size(?DBUS_UINT32)  -> 4;
size(?DBUS_INT64)   -> 8;
size(?DBUS_UINT64)  -> 8;
size(?DBUS_DOUBLE)  -> 8;
size(?DBUS_UNIX_FD) -> 4;
size(_) -> 0.
     
alignment([?DBUS_BYTE|_])             -> 1;
alignment([?DBUS_BOOLEAN|_])          -> 4;
alignment([?DBUS_INT16|_])            -> 2;
alignment([?DBUS_UINT16|_])           -> 2;
alignment([?DBUS_INT32|_])            -> 4;
alignment([?DBUS_UINT32|_])           -> 4;
alignment([?DBUS_INT64|_])            -> 8;
alignment([?DBUS_UINT64|_])           -> 8;
alignment([?DBUS_DOUBLE|_])           -> 8;
alignment([?DBUS_STRING|_])           -> 4;  %% 4-byte length
alignment([?DBUS_OBJPATH|_])          -> 4; %% 4-byte length
alignment([?DBUS_SIGNATURE|_])        -> 1; %% 1-byte length
alignment([?DBUS_ARRAY|_])            -> 4;  %% length
alignment([?DBUS_STRUCT|_])           -> 8;
alignment([?DBUS_STRUCT_BEGIN|_])     -> 8;
alignment([?DBUS_VARIANT|_])          -> 1;
alignment([?DBUS_DICT_ENTRY|_])       -> 8;
alignment([?DBUS_DICT_ENTRY_BEGIN|_]) -> 8;
alignment([?DBUS_UNIX_FD|_])          -> 4.

is_signed(X, N) ->
    ?is_signed(X, N).

is_unsigned(X, N) ->
    ?is_unsigned(X, N).

%% @doc
%%   Encode a sequence of data
%% @end
encode_signature(Signature, Sequence) when is_list(Sequence) ->
    encode_signature(erlang:system_info(endian), 0, Signature, Sequence).

encode_signature(little, Y, Signature, Sequence) ->
    encode_signature_little(Y, Signature, Sequence);
encode_signature(big, Y, Signature, Sequence) ->
    encode_signature_big(Y, Signature, Sequence).


encode_signature_little(Y, Es, [X|Xs]) when is_list(Es) ->
    {E,Es1} = next_argument(Es),
    {V,Y1}  = encode_little(Y,E,X),
    {Vs,Y2} = encode_signature_little(Y1,Es1,Xs),
    {[V|Vs],Y2};
encode_signature_little(Y, _Es, []) ->
    {[],Y}.

encode_signature_big(Y, Es, [X|Xs]) when is_list(Es) ->
    {E,Es1} = next_argument(Es),
    {V,Y1}  = encode_big(Y,E,X),
    {Vs,Y2} = encode_signature_big(Y1,Es1,Xs),
    {[V|Vs],Y2};
encode_signature_big(Y, _Es, []) ->
    {[],Y}.


encode(Signature,Value) ->
    encode(erlang:system_info(endian),Signature,Value).

encode(Endian,Signature,Value) ->
    encode(Endian,0,Signature,Value).

encode(little,Y,Signature,Value) ->
    encode_little(Y,Signature,Value);
encode(big,Y,Signature,Value) ->
    encode_big(Y,Signature,Value).

%% encode little endian data
encode_little(Y,[?DBUS_BYTE], X) when is_integer(X) ->
    {<<X:8>>,Y+1};
encode_little(Y,[?DBUS_BOOLEAN],true) ->
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_UINT(little,1,32)),Y+P0+4};
    
encode_little(Y,[?DBUS_BOOLEAN],false) -> 
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_UINT(little,0,32)),Y+P0+4};

encode_little(Y,[?DBUS_UINT16],X) when is_integer(X) ->
    P0 = pad_size(Y, 2),
    {?PAD_BINARY(P0,?ENCODE_UINT(little,X,16)),Y+P0+2};

encode_little(Y,[?DBUS_UINT32],X) when is_integer(X) ->
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_UINT(little,X,32)),Y+P0+4};

encode_little(Y,[?DBUS_UINT64],X) when is_integer(X) ->
    P0 = pad_size(Y, 8),
    {?PAD_BINARY(P0,?ENCODE_UINT(little,X,64)),Y+P0+8};

encode_little(Y,[?DBUS_INT16],X) when is_integer(X) ->
    P0 = pad_size(Y, 2),
    {?PAD_BINARY(P0,?ENCODE_INT(little,X,16)),Y+P0+2};

encode_little(Y,[?DBUS_INT32],X) when is_integer(X) ->
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_INT(little,X,32)),Y+P0+4};

encode_little(Y,[?DBUS_INT64],X) when is_integer(X) ->
    P0 = pad_size(Y, 8),
    {?PAD_BINARY(P0,?ENCODE_INT(little,X,64)),Y+P0+8};

encode_little(Y,[?DBUS_DOUBLE],X) when is_float(X) ->
    P0 = pad_size(Y, 8),
    {?PAD_BINARY(P0,?ENCODE_FLOAT(little,X,64)), Y+P0+8};

encode_little(Y,[?DBUS_STRING],X) when is_list(X) ->
    P0 = pad_size(Y,4),
    String = unicode:characters_to_binary(X),
    Size   = byte_size(String),
    if Size > ?MAX_STRING_SIZE -> erlang:error(string_too_long);
       true  -> ok
    end,
    {[<<?PAD(P0)>>, ?ENCODE_UINT(little,Size,32), <<String/binary, 0>>],
     Y+P0+4+Size+1};

encode_little(Y,[?DBUS_OBJPATH],X) when is_list(X) ->
    case is_valid_objpath(X) of
	false -> erlang:error(not_an_objpath);
	true -> ok
    end,
    P0 = pad_size(Y,4),
    String = unicode:characters_to_binary(X),
    Size   = byte_size(String),
    if Size > ?MAX_OBJPATH_SIZE -> erlang:error(objpath_too_long);
       true  -> ok
    end,
    {[<<?PAD(P0)>>, ?ENCODE_UINT(little,Size,32), <<String/binary, 0>>],
     Y+P0+4+Size+1};

encode_little(Y,[?DBUS_SIGNATURE],X) when is_list(X) -> 
    String = unicode:characters_to_binary(X),
    Size   = byte_size(String),
    if Size > ?MAX_SIGNATURE_SIZE -> erlang:error(signature_too_long);
       true  -> ok
    end,
    {[<<Size:8>>,<<String/binary, 0>>], Y+1+Size+1};

encode_little(Y,[?DBUS_ARRAY|Es], X) when is_list(X) ->
    {AEs,""} = next_argument(Es),  %% element type
    {Vs,YSz} = encode_array_elements_little(0,AEs,X),
    if YSz > ?MAX_ARRAY_SIZE -> erlang:error(array_too_long);
       true -> ok
    end,
    {Vl,Y1} = encode_little(Y,[?DBUS_UINT32],YSz),
    A = alignment(AEs),
    P0 = pad_size(Y1,A),
    {[Vl,<<?PAD(P0)>>|Vs],Y1+P0+YSz};

encode_little(Y,[?DBUS_STRUCT_BEGIN|Es1], X) when is_tuple(X) ->
    P0 = pad_size(Y, 8),
    %% FIXME: probably record if correct signature?
    encode_struct_little(Y+P0,Es1,1,X,[<<?PAD(P0)>>]);

encode_little(Y,[?DBUS_DICT_ENTRY_BEGIN|Es], {K,V}) ->
    P0 = pad_size(Y, 8),
    {KSpec,Es1} = next_argument(Es),
    {VSpec,[?DBUS_DICT_ENTRY_END]} = next_argument(Es1),
    {KBin,Y1} = encode_little(Y+P0,KSpec,K),
    {VBin,Y2} = encode_little(Y1,VSpec,V),
    {[<<?PAD(P0)>>,KBin,VBin], Y2};

encode_little(Y,[?DBUS_VARIANT], {Signature,Value}) ->
    %% Fixme: handle variant depth !
    {SBin,Y1} = encode_little(Y,[?DBUS_SIGNATURE],Signature),
    {VBin,Y2} = encode_little(Y1,Signature,Value),
    {[SBin,VBin], Y2}.

encode_array_elements_little(Y,Es,[X|Xs]) ->
    {V,Y1} = encode_little(Y,Es,X),
    {Vs,Y2} = encode_array_elements_little(Y1,Es,Xs),
    {[V|Vs], Y2};
encode_array_elements_little(Y,_Es,[]) ->
    {[],Y}.

encode_struct_little(Y,Es,I,X,Acc) ->
    case next_argument(Es) of
	{")",""}   -> 
	    {lists:reverse(Acc),Y};
	{Spec,Es1} ->
	    {Data,Y1} = encode_little(Y,Spec,element(I,X)),
	    encode_struct_little(Y1,Es1,I+1,X,[Data|Acc])
    end.


%% encode big endian data
encode_big(Y,[?DBUS_BYTE], X) when is_integer(X) ->
    {<<X:8>>,Y+1};
encode_big(Y,[?DBUS_BOOLEAN],true) ->
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_UINT(big,1,32)),Y+P0+4};
    
encode_big(Y,[?DBUS_BOOLEAN],false) -> 
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_UINT(big,0,32)),Y+P0+4};

encode_big(Y,[?DBUS_UINT16],X) when is_integer(X) ->
    P0 = pad_size(Y, 2),
    {?PAD_BINARY(P0,?ENCODE_UINT(big,X,16)),Y+P0+2};

encode_big(Y,[?DBUS_UINT32],X) when is_integer(X) ->
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_UINT(big,X,32)),Y+P0+4};

encode_big(Y,[?DBUS_UINT64],X) when is_integer(X) ->
    P0 = pad_size(Y, 8),
    {?PAD_BINARY(P0,?ENCODE_UINT(big,X,64)),Y+P0+8};

encode_big(Y,[?DBUS_INT16],X) when is_integer(X) ->
    P0 = pad_size(Y, 2),
    {?PAD_BINARY(P0,?ENCODE_INT(big,X,16)),Y+P0+2};

encode_big(Y,[?DBUS_INT32],X) when is_integer(X) ->
    P0 = pad_size(Y, 4),
    {?PAD_BINARY(P0,?ENCODE_INT(big,X,32)),Y+P0+4};

encode_big(Y,[?DBUS_INT64],X) when is_integer(X) ->
    P0 = pad_size(Y, 8),
    {?PAD_BINARY(P0,?ENCODE_INT(big,X,64)),Y+P0+8};

encode_big(Y,[?DBUS_DOUBLE],X) when is_float(X) ->
    P0 = pad_size(Y, 8),
    {?PAD_BINARY(P0,?ENCODE_FLOAT(big,X,64)), Y+P0+8};

encode_big(Y,[?DBUS_STRING],X) when is_list(X) ->
    P0 = pad_size(Y,4),
    String = unicode:characters_to_binary(X),
    Size   = byte_size(String),
    if Size > ?MAX_STRING_SIZE -> erlang:error(string_too_long);
       true  -> ok
    end,
    {[<<?PAD(P0)>>, ?ENCODE_UINT(big,Size,32), <<String/binary, 0>>],
     Y+P0+4+Size+1};

encode_big(Y,[?DBUS_OBJPATH],X) when is_list(X) ->
    case is_valid_objpath(X) of
	false -> erlang:error(not_an_objpath);
	true -> ok
    end,
    P0 = pad_size(Y,4),
    String = unicode:characters_to_binary(X),
    Size   = byte_size(String),
    if Size > ?MAX_OBJPATH_SIZE -> erlang:error(objpath_too_long);
       true  -> ok
    end,
    {[<<?PAD(P0)>>, ?ENCODE_UINT(big,Size,32), <<String/binary, 0>>],
     Y+P0+4+Size+1};

encode_big(Y,[?DBUS_SIGNATURE],X) when is_list(X) -> 
    String = unicode:characters_to_binary(X),
    Size   = byte_size(String),
    if Size > ?MAX_SIGNATURE_SIZE -> erlang:error(signature_too_long);
       true  -> ok
    end,
    {[<<Size:8>>,<<String/binary, 0>>], Y+1+Size+1};

encode_big(Y,[?DBUS_ARRAY|Es], X) when is_list(X) ->
    {AEs,""} = next_argument(Es),  %% element type
    {Vs,YSz} = encode_array_elements_big(0,AEs,X),
    if YSz > ?MAX_ARRAY_SIZE -> erlang:error(array_too_long);
       true -> ok
    end,
    {Vl,Y1} = encode_big(Y,[?DBUS_UINT32],YSz),
    A = alignment(AEs),
    P0 = pad_size(Y1,A),
    {[Vl,<<?PAD(P0)>>|Vs],Y1+P0+YSz};

encode_big(Y,[?DBUS_STRUCT_BEGIN|Es1], X) when is_tuple(X) ->
    P0 = pad_size(Y, 8),
    %% FIXME: probably record if correct signature?
    encode_struct_big(Y+P0,Es1,1,X,[<<?PAD(P0)>>]);

encode_big(Y,[?DBUS_DICT_ENTRY_BEGIN|Es], {K,V}) ->
    P0 = pad_size(Y, 8),
    {KSpec,Es1} = next_argument(Es),
    {VSpec,[?DBUS_DICT_ENTRY_END]} = next_argument(Es1),
    {KBin,Y1} = encode_big(Y+P0,KSpec,K),
    {VBin,Y2} = encode_big(Y1,VSpec,V),
    {[<<?PAD(P0)>>,KBin,VBin], Y2};

encode_big(Y,[?DBUS_VARIANT], {Signature,Value}) ->
    %% Fixme: handle variant depth !
    {SBin,Y1} = encode_big(Y,[?DBUS_SIGNATURE],Signature),
    {VBin,Y2} = encode_big(Y1,Signature,Value),
    {[SBin,VBin], Y2}.

encode_array_elements_big(Y,Es,[X|Xs]) ->
    {V,Y1} = encode_big(Y,Es,X),
    {Vs,Y2} = encode_array_elements_big(Y1,Es,Xs),
    {[V|Vs], Y2};
encode_array_elements_big(Y,_Es,[]) ->
    {[],Y}.

encode_struct_big(Y,Es,I,X,Acc) ->
    case next_argument(Es) of
	{")",""}   -> 
	    {lists:reverse(Acc),Y};
	{Spec,Es1} ->
	    {Data,Y1} = encode_big(Y,Spec,element(I,X)),
	    encode_struct_big(Y1,Es1,I+1,X,[Data|Acc])
    end.


%% @doc
%%   Decode a signature
%% @end
decode_signature(little, Y, Es, Bin) ->
    decode_signature_little(Y, Es, Bin);
decode_signature(big, Y, Es, Bin) ->
    decode_signature_big(Y, Es, Bin).


decode_signature_little(Y, [], Bin0) ->
    {[],Y,Bin0};
decode_signature_little(Y, Es, Bin0) ->
    {Spec,Es1} = next_argument(Es),
    {X,Y1,Bin1} = decode_little(Y, Spec, Bin0),
    {Xs,Y2,Bin2} = decode_signature_little(Y1,Es1,Bin1),
    {[X|Xs],Y2,Bin2}.

decode_signature_big(Y, [], Bin0) ->
    {[],Y,Bin0};
decode_signature_big(Y, Es, Bin0) ->
    {Spec,Es1} = next_argument(Es),
    {X,Y1,Bin1} = decode_big(Y, Spec, Bin0),
    {Xs,Y2,Bin2} = decode_signature_big(Y1,Es1,Bin1),
    {[X|Xs],Y2,Bin2}.


decode(little, Y, Type, Bin) ->
    decode_little(Y, Type, Bin);
decode(big, Y, Type, Bin) ->
    decode_big(Y, Type, Bin).


decode_little(Y, [?DBUS_BYTE], Bin) ->
    case Bin of
	<<X:8,T/binary>> -> {X,Y+1,T};
	<<>> -> erlang:error(more_data)
    end;
decode_little(Y,[?DBUS_BOOLEAN],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/little-unsigned-integer,T/binary>> = Bin,
    {X =/= 0,Y+P0+4,T};
decode_little(Y,[?DBUS_UINT16],Bin) ->
    P0 = pad_size(Y,2),
    ?NEED_SIZE(Bin, 2+P0),
    <<?PAD(P0),X:16/little-unsigned-integer,T/binary>> = Bin,
    {X,Y+P0+2,T};
decode_little(Y,[?DBUS_UINT32],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/little-unsigned-integer,T/binary>> = Bin,
    {X,Y+P0+4,T};
decode_little(Y,[?DBUS_UINT64],Bin) ->
    P0 = pad_size(Y,8),
    ?NEED_SIZE(Bin, 8+P0),
    <<?PAD(P0),X:64/little-unsigned-integer,T/binary>> = Bin,
    {X,Y+P0+8,T};
decode_little(Y,[?DBUS_INT16],Bin) ->
    P0 = pad_size(Y,2),
    ?NEED_SIZE(Bin, 2+P0),
    <<?PAD(P0),X:16/little-signed-integer,T/binary>> = Bin,
    {X,Y+P0+2,T};
decode_little(Y,[?DBUS_INT32],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/little-signed-integer,T/binary>> = Bin,
    {X,Y+P0+4,T};
decode_little(Y,[?DBUS_INT64],Bin) ->
    P0 = pad_size(Y,8),
    ?NEED_SIZE(Bin, 8+P0),
    <<?PAD(P0),X:64/little-signed-integer,T/binary>> = Bin,
    {X,Y+P0+8,T};
decode_little(Y,[?DBUS_DOUBLE],Bin) ->
    P0 = pad_size(Y,8),
    ?NEED_SIZE(Bin,8+P0),
    <<?PAD(P0),X:64/little-float,T/binary>> = Bin,
    {X,Y+P0+8,T};
decode_little(Y,[?DBUS_STRING],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/little-unsigned-integer,Bin1/binary>> = Bin,
    ?NEED_SIZE(Bin1,X+1),
    <<Data:X/binary,0,T/binary>> = Bin1,
    {unicode:characters_to_list(Data),Y+P0+4+X+1,T};
decode_little(Y,[?DBUS_OBJPATH],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/little-unsigned-integer,Bin1/binary>> = Bin,
    ?NEED_SIZE(Bin1,X+1),
    <<Data:X/binary,0,T/binary>> = Bin1,
    {unicode:characters_to_list(Data),Y+P0+4+X+1,T};
decode_little(Y,[?DBUS_SIGNATURE],Bin) ->
    ?NEED_SIZE(Bin, 1),
    <<X:8,Bin1/binary>> = Bin,
    ?NEED_SIZE(Bin1,X+1),
    <<Data:X/binary,0,T/binary>> = Bin1,
    {unicode:characters_to_list(Data),Y+1+X+1,T};
decode_little(Y,[?DBUS_ARRAY|Es], Bin) ->
    {AEs,""} = next_argument(Es),  %% element type
    {Size,Y1,Bin1} = decode_little(Y,[?DBUS_UINT32],Bin),
    A = alignment(AEs),
    P0 = pad_size(Y1,A),
    ?NEED_SIZE(Bin1, P0+Size),
    <<?PAD(P0),Bin2:Size/binary,T/binary>> = Bin1,
    {Elems,_,<<>>} = decode_array_elements_little(0,AEs,Bin2),
    {Elems,Y1+P0+Size,T};
decode_little(Y,[?DBUS_STRUCT_BEGIN|Es], Bin) ->
    P0 = pad_size(Y, 8),
    <<?PAD(P0), Bin1/binary>> = Bin,
    decode_struct_little(Y+P0,Es,Bin1,[]);
decode_little(Y,[?DBUS_DICT_ENTRY_BEGIN|Es],Bin) ->
    P0 = pad_size(Y, 8),
    <<?PAD(P0), Bin1/binary>> = Bin,
    {KSpec,Es1} = next_argument(Es),
    {VSpec,[?DBUS_DICT_ENTRY_END]} = next_argument(Es1),
    {K,Y1,Bin2} = decode_little(Y+P0,KSpec,Bin1),
    {V,Y2,Bin3} = decode_little(Y1,VSpec,Bin2),
    {{K,V},Y2,Bin3};
decode_little(Y,[?DBUS_VARIANT],Bin) ->
    {Signature,Y1,Bin1} = decode_little(Y,[?DBUS_SIGNATURE],Bin),
    {Value,Y2,Bin2} = decode_little(Y1,Signature,Bin1),
    {{Signature,Value},Y2,Bin2}.


decode_array_elements_little(Y,_Es, <<>>) ->
    {[],Y,<<>>};
decode_array_elements_little(Y, Es, Bin) ->
    {X,Y1,Bin1} = decode_little(Y, Es, Bin),
    {Xs,Y2,Bin2} = decode_array_elements_little(Y1,Es,Bin1),
    {[X|Xs],Y2,Bin2}.

decode_struct_little(Y,Es,Bin0,Acc) ->
    case next_argument(Es) of
	{")",""} ->
	    {list_to_tuple(lists:reverse(Acc)),Y,Bin0};
	{Spec,Es1} ->
	    {Elem,Y1,Bin1} = decode_little(Y,Spec,Bin0),
	    decode_struct_little(Y1,Es1,Bin1,[Elem|Acc])
    end.
%%
%% Decode big-endian data
%%

decode_big(Y, [?DBUS_BYTE], Bin) ->
    case Bin of
	<<X:8,T/binary>> -> {X,Y+1,T};
	<<>> -> erlang:error(more_data)
    end;
decode_big(Y,[?DBUS_BOOLEAN],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/big-unsigned-integer,T/binary>> = Bin,
    {X =/= 0,Y+P0+4,T};
decode_big(Y,[?DBUS_UINT16],Bin) ->
    P0 = pad_size(Y,2),
    ?NEED_SIZE(Bin, 2+P0),
    <<?PAD(P0),X:16/big-unsigned-integer,T/binary>> = Bin,
    {X,Y+P0+2,T};
decode_big(Y,[?DBUS_UINT32],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/big-unsigned-integer,T/binary>> = Bin,
    {X,Y+P0+4,T};
decode_big(Y,[?DBUS_UINT64],Bin) ->
    P0 = pad_size(Y,8),
    ?NEED_SIZE(Bin, 8+P0),
    <<?PAD(P0),X:64/big-unsigned-integer,T/binary>> = Bin,
    {X,Y+P0+8,T};
decode_big(Y,[?DBUS_INT16],Bin) ->
    P0 = pad_size(Y,2),
    ?NEED_SIZE(Bin, 2+P0),
    <<?PAD(P0),X:16/big-signed-integer,T/binary>> = Bin,
    {X,Y+P0+2,T};
decode_big(Y,[?DBUS_INT32],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/big-signed-integer,T/binary>> = Bin,
    {X,Y+P0+4,T};
decode_big(Y,[?DBUS_INT64],Bin) ->
    P0 = pad_size(Y,8),
    ?NEED_SIZE(Bin, 8+P0),
    <<?PAD(P0),X:64/big-signed-integer,T/binary>> = Bin,
    {X,Y+P0+8,T};
decode_big(Y,[?DBUS_DOUBLE],Bin) ->
    P0 = pad_size(Y,8),
    ?NEED_SIZE(Bin,8+P0),
    <<?PAD(P0),X:64/big-float,T/binary>> = Bin,
    {X,Y+P0+8,T};
decode_big(Y,[?DBUS_STRING],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/big-unsigned-integer,Bin1/binary>> = Bin,
    ?NEED_SIZE(Bin1,X+1),
    <<Data:X/binary,0,T/binary>> = Bin1,
    {unicode:characters_to_list(Data),Y+P0+4+X+1,T};
decode_big(Y,[?DBUS_OBJPATH],Bin) ->
    P0 = pad_size(Y,4),
    ?NEED_SIZE(Bin, 4+P0),
    <<?PAD(P0),X:32/big-unsigned-integer,Bin1/binary>> = Bin,
    ?NEED_SIZE(Bin1,X+1),
    <<Data:X/binary,0,T/binary>> = Bin1,
    {unicode:characters_to_list(Data),Y+P0+4+X+1,T};
decode_big(Y,[?DBUS_SIGNATURE],Bin) ->
    ?NEED_SIZE(Bin, 1),
    <<X:8,Bin1/binary>> = Bin,
    ?NEED_SIZE(Bin1,X+1),
    <<Data:X/binary,0,T/binary>> = Bin1,
    {unicode:characters_to_list(Data),Y+1+X+1,T};
decode_big(Y,[?DBUS_ARRAY|Es], Bin) ->
    {AEs,""} = next_argument(Es),  %% element type
    {Size,Y1,Bin1} = decode_big(Y,[?DBUS_UINT32],Bin),
    A = alignment(AEs),
    P0 = pad_size(Y1,A),
    ?NEED_SIZE(Bin1, P0+Size),
    <<?PAD(P0),Bin2:Size/binary,T/binary>> = Bin1,
    {Elems,_,<<>>} = decode_array_elements_big(0,AEs,Bin2),
    {Elems,Y1+P0+Size,T};
decode_big(Y,[?DBUS_STRUCT_BEGIN|Es], Bin) ->
    P0 = pad_size(Y, 8),
    <<?PAD(P0), Bin1/binary>> = Bin,
    decode_struct_big(Y+P0,Es,Bin1,[]);
decode_big(Y,[?DBUS_DICT_ENTRY_BEGIN|Es],Bin) ->
    P0 = pad_size(Y, 8),
    <<?PAD(P0), Bin1/binary>> = Bin,
    {KSpec,Es1} = next_argument(Es),
    {VSpec,[?DBUS_DICT_ENTRY_END]} = next_argument(Es1),
    {K,Y1,Bin2} = decode_big(Y+P0,KSpec,Bin1),
    {V,Y2,Bin3} = decode_big(Y1,VSpec,Bin2),
    {{K,V},Y2,Bin3};
decode_big(Y,[?DBUS_VARIANT],Bin) ->
    {Signature,Y1,Bin1} = decode_big(Y,[?DBUS_SIGNATURE],Bin),
    {Value,Y2,Bin2} = decode_big(Y1,Signature,Bin1),
    {{Signature,Value},Y2,Bin2}.


decode_array_elements_big(Y,_Es, <<>>) ->
    {[],Y,<<>>};
decode_array_elements_big(Y, Es, Bin) ->
    {X,Y1,Bin1} = decode_big(Y, Es, Bin),
    {Xs,Y2,Bin2} = decode_array_elements_big(Y1,Es,Bin1),
    {[X|Xs],Y2,Bin2}.

decode_struct_big(Y,Es,Bin0,Acc) ->
    case next_argument(Es) of
	{")",""} ->
	    {list_to_tuple(lists:reverse(Acc)),Y,Bin0};
	{Spec,Es1} ->
	    {Elem,Y1,Bin1} = decode_big(Y,Spec,Bin0),
	    decode_struct_big(Y1,Es1,Bin1,[Elem|Acc])
    end.

%%
%% @doc
%%   Fetch next argument in type spec.
%%   Examples:
%%   next_argument("ii") -> {"i", "i"}
%%   next_argument("a{i(ddd)}i") -> {"a{i(ddd)}", "i"}
%% @end
-spec next_argument(Spec::string()) -> {string(), string()}.

next_argument([?DBUS_STRUCT_BEGIN|Es]) ->
    next_argument(Es, [?DBUS_STRUCT_END], [?DBUS_STRUCT_BEGIN]);
next_argument([?DBUS_DICT_ENTRY_BEGIN|Es]) ->
    next_argument(Es, [?DBUS_DICT_ENTRY_END], [?DBUS_DICT_ENTRY_BEGIN]);
next_argument([?DBUS_ARRAY|Es]) ->
    {A,Es1} = next_argument(Es),
    {[?DBUS_ARRAY|A],Es1};
next_argument([E|Es]) ->
    {[E], Es}.

next_argument([E|Es], [E], Acc) ->
    {lists:reverse([E|Acc]), Es};
next_argument([E|Es], [E|Fs], Acc) ->
    next_argument(Es, Fs, [E|Acc]);
next_argument([?DBUS_STRUCT_BEGIN|Es], Fs, Acc) ->
    next_argument(Es, [?DBUS_STRUCT_END|Fs], [?DBUS_STRUCT_BEGIN|Acc]);
next_argument([?DBUS_DICT_ENTRY_BEGIN|Es], Fs, Acc) ->
    next_argument(Es, [?DBUS_DICT_ENTRY_END|Fs], [?DBUS_DICT_ENTRY_BEGIN|Acc]);
next_argument([E|Es], Fs=[_|_], Acc) ->
    next_argument(Es, Fs, [E|Acc]).

%%
%% Convert a "readable" symbolic type spec into a signature
%%
type_spec_to_signature(Spec) ->
    case basic_spec_to_signature(Spec) of
	[] ->
	    case Spec of
		{dict,Key,Value} ->
		    [KSig] = basic_spec_to_signature(Key),
		    VSig = type_spec_to_signature(Value),
		    [?DBUS_ARRAY,
		     ?DBUS_DICT_ENTRY_BEGIN,KSig]++VSig++
			[?DBUS_DICT_ENTRY_END];
		{array,Elem} ->
		    [?DBUS_ARRAY|type_spec_to_signature(Elem)];
		{struct,Es} ->
		    lists:append([[?DBUS_STRUCT_BEGIN] |
				  lists:map(fun type_spec_to_signature/1, Es)])
			++ [?DBUS_STRUCT_END]
	    end;
	Sig -> Sig
    end.

basic_spec_to_signature(Spec) ->
    case Spec of
	byte    -> [?DBUS_BYTE];
	boolean -> [?DBUS_BOOLEAN];
	int16   -> [?DBUS_INT16];
	uint16  -> [?DBUS_UINT16];
	int32   -> [?DBUS_INT32];
	uint32  -> [?DBUS_UINT32];
	int64   -> [?DBUS_INT64];
	uint64  -> [?DBUS_UINT64];
	double  -> [?DBUS_DOUBLE];
	unix_fd -> [?DBUS_UNIX_FD];
	string  -> [?DBUS_STRING];
	variant -> [?DBUS_VARIANT];
	objpath -> [?DBUS_OBJPATH];
	_ -> []
    end.


%%
%% re:run(Path, "(/[A-Za-z0-9_]+)+")
%%

is_valid_objpath([$/]) ->
    true;
is_valid_objpath([$/|Cs]) ->
    is_valid_objpath_1(Cs);
is_valid_objpath(_) ->
    false.

is_valid_objpath_1([C|Cs]) ->
    if C >= $A, C =< $Z -> is_valid_objpath_2(Cs);
       C >= $a, C =< $z -> is_valid_objpath_2(Cs);
       C >= $0, C =< $9 -> is_valid_objpath_2(Cs);
       C =:= $_ -> is_valid_objpath_2(Cs);
       true -> false
    end;
is_valid_objpath_1([]) ->
    false.

is_valid_objpath_2([$/|Cs]) ->
    is_valid_objpath_1(Cs);
is_valid_objpath_2([C|Cs]) ->
    if C >= $A, C =< $Z -> is_valid_objpath_2(Cs);
       C >= $a, C =< $z -> is_valid_objpath_2(Cs);
       C >= $0, C =< $9 -> is_valid_objpath_2(Cs);
       C =:= $_ -> is_valid_objpath_2(Cs);
       true -> false
    end;
is_valid_objpath_2([]) ->
    true.


is_valid_interface_name(_Interface) ->
    %% <interface-name> = <elem> ( '.' <elem> )*
    %% <elem> = [A-Za-z_][A-Za-z_0-9]*
    %%
    true.

is_validate_bus_name(_Bus) ->
    %% <bus-name> = ':' <uname> | <name>
    %% <uname> = <uelem> '.' <uelem> ( '.' <uelem> )*
    %% <name> = <elem> '.' <elem> ( '.' <elem> )*
    %% <uelem> = [A-Za-z_-0-9]+
    %% <elem> = [A-Za-z_-][A-Za-z_-0-9]*
    %%
    true.

is_valid_member_name(_Member) ->
    %% <member-name> = [A-Za-z_][A-Za-z0-9_]*
    true.

validate_class($d, C) -> 
    ((C >= $0) andalso (C =< $9));
validate_class($h, C) ->
    case C of
	$\s -> true;
	$\t -> true;
	_ -> false
    end;
validate_class($s, C) -> 
    case C of
	$\t -> true;
	$\n -> true;
	$\f -> true;
	$\r -> true;
	$\s -> true;
	_ -> false
    end;
validate_class($w, C) -> 
    ((C >= $a) andalso (C =< $z)) orelse
    ((C >= $A) andalso (C =< $Z)) orelse
    ((C >= $0) andalso (C =< $9)) orelse (C =:= $_);

validate_class($D, C) -> not validate_class($d, C);
validate_class($H, C) -> not validate_class($h, C);
validate_class($S, C) -> not validate_class($s, C);
validate_class($W, C) -> not validate_class($w, C).


validate_string([$\\,X,$+|Xs], [C|Cs]) ->
    case validate_class(X, C) of
	true -> validate_string([$\\,X,$*|Xs],Cs);
	false -> false
    end;
validate_string(Xs0=[$\\,X,$*|Xs1], Cs0=[C|Cs]) ->
    case validate_class(X, C) of
	true -> validate_string(Xs0,Cs);
	false -> validate_string(Xs1,Cs0)
    end;
validate_string([$\\,X|Xs1], [C|Cs]) ->
    case validate_class(X, C) of
	true -> validate_string(Xs1,Cs);
	false -> false
    end;
validate_string([C,$+|Xs], [C|Cs]) ->
    validate_string([C,$*|Xs],Cs);
validate_string(Xs0=[C,$*|_], [C|Cs]) ->
    validate_string(Xs0,Cs);
validate_string([C|Xs], [C|Cs]) ->
    validate_string(Xs,Cs);
validate_string([],[])->
    true;
validate_string(_,_) ->
    false.

%%
%% Testing
%%
perf() ->
    perf(1000).

perf(N) ->
    Spec = "(ya(nqiuxt)a{su}dsov)",
    V = {100,[{1,2,3,4,5,6},
	      {7,8,9,10,11,12},
	      {13,14,15,16,17,18},
	      {19,20,21,22,23,24}],
	 [{"a",1},{"b",2},{"c",3}],
	 3.14,"hello","/foo/bar", {"s","test"}},
    Endian = big,
    perf_data(N, Endian, Spec, V).

perf_array(N) ->
    Spec = "ay",  %% byte array
    V = lists:seq(0,255),
    Endian = little,
    perf_data(N, Endian, Spec, V).


perf_data(N, Endian, Spec, Value) ->
    test_type(Endian, Spec, Value),

    T0 = os:timestamp(),
    perf_encode(N, Endian, Spec, Value),
    T1 = os:timestamp(),

    {IOList,_Y1} = encode(Endian, 0, Spec, Value),
    Bin = iolist_to_binary(IOList),
    T2 = os:timestamp(),
    perf_decode(N, Endian, Spec, Bin),
    T3 = os:timestamp(),
    
    { (N / timer:now_diff(T1,T0))*1000000,
      (N / timer:now_diff(T3,T2))*1000000}.
      

perf_encode(0, _E, _Spec, _V) ->
    ok;
perf_encode(I, E, Spec, V) ->
    encode(E, 0, Spec, V),
    perf_encode(I-1,E,Spec,V).

perf_decode(0, _E, _Spec, _Bin) ->
    ok;
perf_decode(I, E, Spec, Bin) ->
    decode(E, 0, Spec, Bin),
    perf_decode(I-1,E,Spec,Bin).


    
test() ->
    test_basic(),
    test_struct(),
    test_array(),
    test_variant(),
    ok.

test_basic() ->
    test_type("y", 0),
    test_type("y", 1),
    test_type("y", 255),
    test_type("b", true),
    test_type("b", false),

    test_type("n", -(1 bsl 15)),
    test_type("n", (1 bsl 15)-1),
    test_type("q", 1),
    test_type("q", (1 bsl 16)-1),

    test_type("i", -(1 bsl 31)),
    test_type("i", (1 bsl 31)-1),
    test_type("u", 1),
    test_type("u", (1 bsl 32)-1),

    test_type("x", -(1 bsl 63)),
    test_type("x", (1 bsl 63)-1),
    test_type("t", 1),
    test_type("t", (1 bsl 64)-1),

    test_type("d", -3.14),
    test_type("d", 1000.0001),
    
    test_type("s", "Hello world"),
    test_type("s", ""),
    test_type("s", "foo"),

    test_type("o", "/com/feuerlabs/spec/1_0"),

    test_type("g", "((ii)(id)(is))"),
    ok.

test_struct() ->
    test_type("(i)", {1}),
    test_type("(ii)", {1,2}),
    test_type("(iii)", {1,2,3}),
    test_type("(i(ii))", {1,{2,3}}),
    test_type("((ii)i)", {{1,2},3}),
    test_type("(sid)", {"x",1,2.1}).

test_array() ->
    test_type("ai", [1,2,3,4]),
    test_type("ad", [1.0,2.0,3.0,4.0]),
    test_type("a{is}", [{1,"x"},{2,"y"},{3,"z"}]),
    test_type("a(ibs)", [{1,true,"x"},{2,false,"y"}]).

test_variant() ->
    test_type("v", {"(iii)", {1,2,3}}),
    test_type("v", {"d", 2.0}),
    test_type("v", {"ai", [1,2,3,4]}).
    

test_type(Spec, Value) ->
    test_type(little, Spec, Value),
    test_type(big, Spec, Value).

test_type(Endian, Spec, Value) ->
    io:format("test: ~w spec=~s, value=~p\n", [Endian, Spec, Value]),
    P0 = 0,
    {IOList,Y1} = encode(Endian,P0,Spec,Value),
    {Value,Y1,<<>>} = decode(Endian,P0,Spec,iolist_to_binary(IOList)).
