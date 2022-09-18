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
%%%     DBus type codec
%%%     Ref: http://dbus.freedesktop.org/doc/dbus-specification.html
%%% @end
%%% Created :  3 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(dbus_codec).

-include("../include/dbus.hrl").

-export([encode_args/2, encode_args/4,
	 encode/2, encode/3, encode/4,
	 decode_args/4,
	 decode/2, decode/3, decode/4]).
-export([next_arg/1]).
-export([efilter/1]).
-export([type_spec_to_signature/1]).
-export([signature_to_type_spec/1]).
-export([is_valid_interface_name/1,
	 is_valid_bus_name/1,
	 is_valid_member_name/1]).
-export([validate_class/2, validate_string/2]).
-export([size/1]).

-import(lists, [map/2,append/1,reverse/1]).

-define(TEST, true).

-ifdef(TEST).
-export([test/0]).
-export([perf/0]).
-compile(export_all).
-endif.

%% -define(TRACE(F,A), io:format((F),(A))).
-define(TRACE(F,A), ok).

%% -define(DBG(F,A), io:format((F),(A))).
-define(DBG(F,A), ok).

-define(MAX_STRING_SIZE,    16#ffffffff).
-define(MAX_OBJPATH_SIZE,   16#ffffffff).
-define(MAX_SIGNATURE_SIZE, 16#ff).
-define(MAX_ARRAY_SIZE,     16#3ffffff).

%% -compile({inline,[pad_size/2]}).
%% pad_size(Y,N) ->
%%     case (Y band ((N)-1)) of
%% 	0 -> 0;
%% 	R -> N-R
%%     end.

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
alignment([?DBUS_OBJPATH|_])          -> 4;  %% 4-byte length
alignment([?DBUS_SIGNATURE|_])        -> 1;  %% 1-byte length
alignment([?DBUS_ARRAY|_])            -> 4;  %% length
alignment([?DBUS_DICT_ENTRY_BEGIN|_]) -> 8;
alignment([?DBUS_STRUCT_BEGIN|_])     -> 8;
alignment([?DBUS_VARIANT|_])          -> 1;
%% special
alignment([?DBUS_STRUCT|_])           -> 8;
alignment([?DBUS_DICT_ENTRY|_])       -> 8;
alignment([?DBUS_UNIX_FD|_])          -> 4;
%% special
%% alignment([?DBUS_ERLANG|_])           -> 4. %% array binary
alignment([?DBUS_ERLANG|_])           -> 1.  %% variant


%% @doc
%%   Encode a sequence of data
%% @end
encode_args(Signature, Xs) when is_list(Xs) ->
    encode_args(Signature, Xs, 0, erlang:system_info(endian)).

encode_args(Signature, Xs, Y, Endian) ->
    encode_seq(Signature, Xs, Y, Endian).

-spec encode(Signatur::string(), Value::term()) ->
	  {Data::iodata(),Length::integer()}.

encode(Signature,Value) ->
    encode(Signature, Value,0,erlang:system_info(endian)).

-spec encode(Signatur::string(), Value::term(),
	     Endian::little|big) -> {Data::iodata(),Length::integer()}.

encode(Signature,Value,Endian) ->
    encode(Signature,Value,0,Endian).

-spec encode(Signatur::string(), X::term(), Y::integer(),
	     Endian::little|big) -> {Data::iodata(),Length::integer()}.

%% consider re-introduce alignment before all entires	  
encode(Signature, X, Y, Endian) ->
    case Signature of
	[?DBUS_BYTE] ->
	    encode_byte(X, Y, Endian);
	[?DBUS_BOOLEAN] ->
	    case X of
		true -> encode_uint32(1, Y, Endian);
		false -> encode_uint32(0, Y, Endian)
	    end;
	[?DBUS_UINT16] ->
	    encode_uint16(X, Y, Endian);
	[?DBUS_UINT32] ->
	    encode_uint32(X, Y, Endian);
	[?DBUS_UINT64] ->
	    encode_uint64(X, Y, Endian);
	[?DBUS_INT16] ->
	    encode_int16(X, Y, Endian);
	[?DBUS_INT32] ->
	    encode_int32(X, Y, Endian);
	[?DBUS_INT64] ->
	    encode_int64(X, Y, Endian);
	[?DBUS_DOUBLE] ->
	    encode_double(X, Y, Endian);
	[?DBUS_STRING] ->
	    ?TRACE("~w: string:~ts\n", [Y, X]),
	    encode_string(X, Y, ?MAX_STRING_SIZE, Endian);
	[?DBUS_OBJPATH] ->
	    ?TRACE("~w: objpath:~ts\n", [Y, X]),
	    case is_valid_objpath(X) of
		false -> erlang:error(not_an_objpath);
		true -> encode_string(X, Y, ?MAX_OBJPATH_SIZE,Endian)
	    end;
	[?DBUS_SIGNATURE] ->
	    X1 = if is_atom(X) -> atom_to_list(X); 
		    is_list(X) -> X 
		 end,
	    Bytes = unicode:characters_to_binary(X1),
	    Len   = byte_size(Bytes),
	    if Len =< ?MAX_SIGNATURE_SIZE ->
		    ?TRACE("~w: signature:~ts\n", [Y, Bytes]),
		    {[Len,Bytes,0],1+Len+1};
	       true ->	       
		    erlang:error(signature_too_long)
	    end;
	%% special case for binaries = "ay"
	[?DBUS_ARRAY,?DBUS_BYTE] ->
	    Bytes = iolist_to_binary(X),
	    Len = byte_size(Bytes),
	    if Len > ?MAX_ARRAY_SIZE -> erlang:error(array_too_long);
	       true -> ok
	    end,
	    {LenBytes,LenSize} = encode_len(Len,Y,Endian),
	    {[LenBytes,Bytes,0],Len+LenSize};
	%% a{kv}
	[?DBUS_ARRAY,?DBUS_DICT_ENTRY_BEGIN|Es] ->
	    {KSig,Es1} = next_arg(Es),
	    {VSig,[?DBUS_DICT_ENTRY_END]} = next_arg(Es1),
	    Iter = maps:iterator(X),
	    ?TRACE("~w: dict of \"~s\"=>\"~s\" [\n",[Y,KSig,VSig]),
	    LenPad = ?PAD_SIZE(Y,4),     %% pad uint32 length
	    Y0 = Y+LenPad,               %% start array length
	    Y1 = Y0+4,                   %% array start
	    EntPad = ?PAD_SIZE(Y1,8),    %% first entry pad
	    case encode_dict(KSig,VSig,Y1+EntPad,Endian,Iter) of
		{_,VSz} when VSz >= ?MAX_ARRAY_SIZE ->
		    erlang:error(dict_too_large);
		{Vs,VSz} ->
		    {LenBytes,LenSize} = encode_len(VSz,Y,Endian),
		    ?TRACE("~w: size=~w\n", [Y+LenSize+EntPad+VSz,VSz]),
		    {[LenBytes,<<?PAD(EntPad)>>,Vs], LenSize+EntPad+VSz}
	    end;
	[?DBUS_ARRAY|Es] ->
	    {ESig,""} = next_arg(Es),   %% element type
	    ?TRACE("~w: array of \"~s\" [\n", [Y, ESig]),
	    LenPad = ?PAD_SIZE(Y,4),    %% pad uint32 length
	    Y0 = Y+LenPad,              %% y0 is start of UINT32 
	    Y1 = Y0+4,                  %% y1 is after UINT32
	    A  = alignment(ESig),       %% alignment for array
	    EntPad = ?PAD_SIZE(Y1,A),   %% byte alignment to first element
	    case encode_array(ESig,X,Y1+EntPad,Endian) of
		{_,VSz} when VSz > ?MAX_ARRAY_SIZE -> 
		    erlang:error(array_too_long);
		{Vs,VSz} ->
		    {LenBytes,LenSize} = encode_len(VSz,Y,Endian),
		    ?TRACE("~w: size=~w\n", [Y+LenSize+EntPad+VSz,VSz]),
		    {[LenBytes,<<?PAD(EntPad)>>,Vs],LenSize+EntPad+VSz}
	    end;
	[?DBUS_STRUCT_BEGIN|Es1] ->
	    EntPad = ?PAD_SIZE(Y, 8),
	    ?TRACE("~w: struct [\n", [Y]),
	    {SBin,SLen} = encode_struct(Es1,X,Y+EntPad,Endian),
	    ?TRACE("~w: size=~w ]\n", [Y+EntPad+SLen, SLen]),	    
	    {[<<?PAD(EntPad)>>,SBin],EntPad+SLen};

	[?DBUS_DICT_ENTRY_BEGIN|Es] ->
	    {K,V} = X,
	    {KSig,Es1} = next_arg(Es),
	    {VSig,[?DBUS_DICT_ENTRY_END]} = next_arg(Es1),
	    ?TRACE("~w: entry ~s, ~s [\n", [Y,KSig,VSig]),
	    EntPad = ?PAD_SIZE(Y, 8),
	    {KBin,KLen} = encode(KSig,K,Y+EntPad,Endian),
	    Y2 = Y+EntPad+KLen,
	    {VBin,VLen} = encode(VSig,V,Y2,Endian),
	    ?TRACE("~w: size=~w]\n", [Y+KLen+VLen, KLen+VLen]),
	    {[<<?PAD(EntPad)>>,KBin,VBin], EntPad+KLen+VLen};

	[?DBUS_VARIANT] ->
	    {Sig,Val} = X,
	    ?TRACE("~w: variant [\n", [Y]),
	    {SBin,SLen} = encode([?DBUS_SIGNATURE],efilter(Sig),Y,Endian),
	    {VBin,VLen} = encode(Sig,Val,Y+SLen,Endian),
	    ?TRACE("~w: ]\n", [Y+SLen+VLen]),
	    {[SBin,VBin], SLen+VLen};
	[?DBUS_ERLANG] ->
	    Variant = dbus_erlang:evariant(X),
	    encode([?DBUS_VARIANT],Variant,Y,Endian)
    end.

encode_byte(X, _Y, _) ->
    ?TRACE("~w: byte:~w\n", [_Y, X]),
    {<<X:8/unsigned-integer>>, 1}.

encode_int16(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 2),
    ?TRACE("~w: int16:~w\n", [Y, X]),
    {<<?PAD(Pad),X:16/little-signed-integer>>, 2+Pad};
encode_int16(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 2),
    ?TRACE("~w: int16:~w\n", [Y, X]),
    {<<?PAD(Pad),X:16/big-signed-integer>>, 2+Pad}.

encode_uint16(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 2),
    ?TRACE("~w: uint16:~w\n", [Y, X]),
    {<<?PAD(Pad),X:16/little-unsigned-integer>>, 2+Pad};
encode_uint16(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 2),
    ?TRACE("~w: uint16:~w\n", [Y, X]),
    {<<?PAD(Pad),X:16/big-unsigned-integer>>, 2+Pad}.

encode_int32(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 4),
    ?TRACE("~w: int32:~w\n", [Y, X]),
    {<<?PAD(Pad),X:32/little-signed-integer>>, 4+Pad};
encode_int32(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 4),
    ?TRACE("~w: int32:~w\n", [Y, X]),
    {<<?PAD(Pad),X:32/big-signed-integer>>, 4+Pad}.

encode_uint32(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 4),
    ?TRACE("~w: uint32:~w\n", [Y, X]),
    {<<?PAD(Pad),X:32/little-unsigned-integer>>, 4+Pad};
encode_uint32(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 4),
    ?TRACE("~w: uint32:~w\n", [Y, X]),
    {<<?PAD(Pad),X:32/big-unsigned-integer>>, 4+Pad}.

encode_len(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 4),
    ?TRACE("~w: len32:~w\n", [Y, X]),
    {<<?PAD(Pad),X:32/little-unsigned-integer>>, 4+Pad};
encode_len(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 4),
    ?TRACE("~w: len32:~w\n", [Y, X]),
    {<<?PAD(Pad),X:32/big-unsigned-integer>>, 4+Pad}.

encode_uint64(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 8),
    ?TRACE("~w: uint64:~w\n", [Y, X]),
    {<<?PAD(Pad),X:64/little-unsigned-integer>>, 8+Pad};
encode_uint64(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 8),
    ?TRACE("~w: uint64:~w\n", [Y, X]),
    {<<?PAD(Pad),X:64/big-unsigned-integer>>, 8+Pad}.

encode_int64(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 8),
    ?TRACE("~w: int64:~w\n", [Y, X]),
    {<<?PAD(Pad),X:64/little-signed-integer>>, 8+Pad};
encode_int64(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 8),
    ?TRACE("~w: int64:~w\n", [Y, X]),
    {<<?PAD(Pad),X:64/big-signed-integer>>, 8+Pad}.

encode_double(X, Y, little) ->
    Pad = ?PAD_SIZE(Y, 8),
    ?TRACE("~w: double:~w\n", [Y, X]),
    {<<?PAD(Pad),X:64/little-float>>, 8+Pad};
encode_double(X, Y, big) ->
    Pad = ?PAD_SIZE(Y, 8),
    ?TRACE("~w: double:~w\n", [Y, X]),
    {<<?PAD(Pad),X:64/big-float>>, 8+Pad}.

encode_string(X, Y, MaxLen, Endian) ->
    X1 = if is_atom(X) -> atom_to_list(X); 
	    is_list(X) -> X 
	 end,
    Bytes = unicode:characters_to_binary(X1),
    Len   = byte_size(Bytes),
    if Len > MaxLen ->
	    erlang:error(string_too_long);
       true ->
	    {LenBytes,LenSize} = encode_len(Len,Y,Endian),
	    {[LenBytes,Bytes,0],LenSize+Len+1}
    end.

encode_dict(KSig, VSig, Y, Endian, Iter) ->
    encode_dict_(KSig, VSig, Y, Endian, Iter, 0, []).

encode_dict_(KSig, VSig, Y, Endian, Iter, Len, Acc) ->
    case maps:next(Iter) of
	{K,V,Iter1} ->
	    EntPad = ?PAD_SIZE(Y,8),
	    {KBin,KLen} = encode(KSig,K,Y+EntPad,Endian),
	    {VBin,VLen} = encode(VSig,V,Y+EntPad+KLen,Endian),
	    DLen = EntPad+KLen+VLen,
	    encode_dict_(KSig, VSig, Y+DLen, Endian, Iter1, Len+DLen,
			 [[<<?PAD(EntPad)>>,KBin,VBin]|Acc]);
	none ->
	    {lists:reverse(Acc), Len}
    end.

encode_array(Sig,Xs,Y,Endian) ->
    encode_array_(Sig,Xs,Y,Endian,0,[]).

encode_array_(Sig,[X|Xs],Y,Endian,Len,Acc) ->
    {EBin,ELen} = encode(Sig,X,Y,Endian),
    encode_array_(Sig, Xs, Y+ELen,Endian,Len+ELen,
		  [EBin|Acc]);
encode_array_(_Sig,[],_Y,_Endian,Len,Acc) ->
    {reverse(Acc),Len}.

encode_struct(SigList,X,Y,Endian) ->
    encode_struct_(SigList,X,Y,Endian,1,0,[]).

encode_struct_(SigList,X,Y,Endian,I,Len,Acc) ->
    case next_arg(SigList) of
	{")",""}   -> 
	    {reverse(Acc),Len};
	{Sig,SigList1} ->
	    {FBin,FLen} = encode(Sig,element(I,X),Y,Endian),
	    encode_struct_(SigList1,X,Y+FLen,Endian,I+1,Len+FLen,[FBin|Acc])
    end.

encode_seq(SigList,Xs,Y,Endian) ->
    encode_seq_(SigList,Xs,Y,Endian,0,[]).

encode_seq_("",[],_Y,_Endian,Len,Acc) ->
    {reverse(Acc),Len};    
encode_seq_(SigList,[X|Xs],Y,Endian,Len,Acc) ->
    {Sig,SigList1} = next_arg(SigList),
    {FBin,FLen} = encode(Sig,X,Y,Endian),
    encode_seq_(SigList1,Xs,Y+FLen,Endian,Len+FLen,[FBin|Acc]).

efilter([?DBUS_ERLANG|Cs]) ->
    [?DBUS_VARIANT|efilter(Cs)];
    %% [?DBUS_ARRAY,?DBUS_BYTE|efilter(Cs)];
efilter([C|Cs]) ->
    [C|efilter(Cs)];
efilter([]) ->
    [].
    
%% @doc
%%   Decode a signature
%% @end

decode_args(Signature, Bin) ->
    decode_args(Signature, Bin, 0, erlang:system_info(endian)).

decode_args(Signature, Bin, Y, Endian) ->
    decode_seq(Signature, Bin, Y, Endian).

decode(Signature,Bin) ->
    decode(Signature,Bin,0,erlang:system_info(endian)).

decode(Signature,Bin,Endian) ->
    decode(Signature,Bin,0,Endian).

%% consider handle alignment before all entires	  
decode(Signature,Bin,Y,Endian) ->
    case Signature of
	[?DBUS_BYTE] ->
	    decode_byte(Bin, Y, Endian);
	[?DBUS_BOOLEAN] ->
	    case decode_uint32(Bin, Y, Endian) of
		{1, Len, Bin1} -> {true, Len, Bin1};
		{0, Len, Bin1} -> {false,Len, Bin1}
	    end;
	[?DBUS_UINT16] ->
	    decode_uint16(Bin, Y, Endian);
	[?DBUS_UINT32] ->
	    decode_uint32(Bin, Y, Endian);
	[?DBUS_UINT64] ->
	    decode_uint64(Bin, Y, Endian);
	[?DBUS_INT16] ->
	    decode_int16(Bin, Y, Endian);
	[?DBUS_INT32] ->
	    decode_int32(Bin, Y, Endian);
	[?DBUS_INT64] ->
	    decode_int64(Bin, Y, Endian);
	[?DBUS_DOUBLE] ->
	    decode_double(Bin, Y, Endian);
	[?DBUS_STRING] ->
	    decode_string(Bin, Y, Endian);
	[?DBUS_OBJPATH] ->
	    decode_string(Bin, Y, Endian);
	[?DBUS_SIGNATURE] ->
	    <<Size:8,Data:Size/binary,0,Bin1/binary>> = Bin,
	    String = unicode:characters_to_list(Data),
	    ?TRACE("~w: signature:\"~ts\"\n", [Y, String]),
	    {String,1+Size+1,Bin1};

	%% special case for binaries = "ay"
	[?DBUS_ARRAY,?DBUS_BYTE] ->
	    {Size, SizeLen, Bin1} = decode_len(Bin, Y, Endian),
	    <<Bytes:Size/binary, Bin2/binary>> = Bin1,
	    ?TRACE("~w: byte array size=~w, data=~p\n", [Y, Size, Bytes]),
	    {Bytes,SizeLen+Size,Bin2};
	%% a{kv}
	[?DBUS_ARRAY,?DBUS_DICT_ENTRY_BEGIN|Es] ->
	    {KSig,Es1} = next_arg(Es),
	    {VSig,[?DBUS_DICT_ENTRY_END]} = next_arg(Es1),
	    {Size, SizeLen, Bin1} = decode_len(Bin, Y, Endian),
	    ?TRACE("~w: dict of \"~s\"=>\"~s\" size=~w [\n",[Y,KSig,VSig,Size]),
	    Y1 = Y + SizeLen,
	    EntPad = ?PAD_SIZE(Y1,8),
	    <<_:EntPad/binary, Data:Size/binary, Bin2/binary>> = Bin1,
	    {Elems,ElemsLen} = decode_dict(KSig,VSig,Y1+EntPad,Endian,Data),
	    ?TRACE("~w: ]\n", [Y1+EntPad+ElemsLen]),
	    {maps:from_list(Elems),SizeLen+EntPad+ElemsLen, Bin2};
	%% ax
	[?DBUS_ARRAY|Es] ->
	    {ESig,""} = next_arg(Es),  %% element type
	    {Size, SizeLen, Bin1} = decode_len(Bin, Y, Endian),
	    ?TRACE("~w: array of ~s size=~w\n", [Y, ESig, Size]),
	    Y1 = Y+SizeLen,
	    A  = alignment(ESig),      %% alignment for array
	    EntPad = ?PAD_SIZE(Y1,A),
	    ?DBG("align=~w,EntPad:~w, byte_size(Bin1)=~w\n",
		 [A, EntPad, byte_size(Bin1)]),
	    <<_:EntPad/binary, Data:Size/binary, Bin2/binary>> = Bin1,
	    {Elems,ElemsLen} = decode_array(ESig,Data,Y1+EntPad,Endian),
	    ?TRACE("~w: ]\n", [Y1+EntPad+ElemsLen]),
	    {Elems,SizeLen+EntPad+ElemsLen,Bin2};

	[?DBUS_STRUCT_BEGIN|Es] ->
	    EntPad = ?PAD_SIZE(Y, 8),
	    <<?SKIP(EntPad), Bin1/binary>> = Bin,
	    ?TRACE("~w: struct [\n", [Y]),
	    {Elems,ElemsLen,Bin2} = decode_struct(Es,Bin1,Y+EntPad,Endian),
	    ?TRACE("~w: ]\n", [Y+ElemsLen]),
	    {Elems,EntPad+ElemsLen,Bin2};

	[?DBUS_DICT_ENTRY_BEGIN|Es] ->
	    EntPad = ?PAD_SIZE(Y, 8),
	    <<_:EntPad/binary, Bin1/binary>> = Bin,
	    {KSig,Es1} = next_arg(Es),
	    {VSig,[?DBUS_DICT_ENTRY_END]} = next_arg(Es1),
	    ?TRACE("~w: entry ~s, ~s [\n", [Y,KSig,VSig]),
	    {K,KLen,Bin2} = decode(KSig,Bin1,Y+EntPad,Endian),
	    {V,VLen,Bin3} = decode(VSig,Bin2,Y+EntPad+KLen,Endian),
	    ?TRACE("~w: ]\n", [Y+KLen+VLen]),
	    {{K,V},EntPad+KLen+VLen,Bin3};

	[?DBUS_VARIANT] ->
	    ?TRACE("~w: variant [\n", [Y]),
	    {Sig,SLen,Bin1} = decode([?DBUS_SIGNATURE],Bin,Y,Endian),
	    {Val,VLen,Bin2} = decode(Sig,Bin1,Y+SLen,Endian),
	    ?TRACE("~w: ]\n", [Y+SLen+VLen]),
	    {{Sig,Val},SLen+VLen,Bin2};

	[?DBUS_ERLANG] ->
	    decode([?DBUS_VARIANT], Bin, Y, Endian)
    end.

decode_byte(<<X:8, Bin1/binary>>, _Y, _Endian) ->
    ?TRACE("~w: byte:~w\n", [_Y, X]),
    {X, 1, Bin1}.

decode_int16(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 2),
    <<?SKIP(Pad),X:16/little-signed-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: int16:~w\n", [Y, X]),
    {X, 2+Pad, Bin1};
decode_int16(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 2),
    <<?SKIP(Pad),X:16/big-signed-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: int16:~w\n", [Y, X]),
    {X, 2+Pad, Bin1}.

decode_uint16(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 2),
    <<?SKIP(Pad), X:16/little-unsigned-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: uint16:~w\n", [Y, X]),
    {X, 2+Pad, Bin1};
decode_uint16(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 2),
    <<?SKIP(Pad), X:16/big-unsigned-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: uint16:~w\n", [Y, X]),
    {X, 2+Pad, Bin1}.

decode_int32(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 4),
    <<?SKIP(Pad),X:32/little-signed-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: int32:~w\n", [Y, X]),
    {X, 4+Pad, Bin1};
decode_int32(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 4),
    <<?SKIP(Pad),X:32/big-signed-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: int32:~w\n", [Y, X]),
    {X, 4+Pad, Bin1}.

decode_uint32(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 4),
    <<?SKIP(Pad),X:32/little-unsigned-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: uint32:~w\n", [Y, X]),
    {X, 4+Pad, Bin1};
decode_uint32(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 4),
    <<?SKIP(Pad),X:32/big-unsigned-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: uint32:~w\n", [Y, X]),
    {X, 4+Pad, Bin1}.

decode_len(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 4),
    <<?SKIP(Pad),X:32/little-unsigned-integer,Bin1/binary>> = Bin,
    {X, 4+Pad, Bin1};
decode_len(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 4),
    <<?SKIP(Pad),X:32/big-unsigned-integer,Bin1/binary>> = Bin,
    {X, 4+Pad, Bin1}.

decode_int64(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 8),
    <<?SKIP(Pad),X:64/little-signed-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: int64:~w\n", [Y, X]),
    {X, 8+Pad, Bin1};
decode_int64(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 8),
    <<?SKIP(Pad),X:64/big-signed-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: int64:~w\n", [Y, X]),
    {X, 8+Pad, Bin1}.

decode_uint64(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 8),
    <<?SKIP(Pad),X:64/little-unsigned-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: uint64:~w\n", [Y, X]),
    {X, 8+Pad, Bin1};
decode_uint64(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 8),
    <<?SKIP(Pad),X:64/big-unsigned-integer,Bin1/binary>> = Bin,
    ?TRACE("~w: uint64:~w\n", [Y, X]),
    {X, 8+Pad, Bin1}.

decode_double(Bin, Y, little) ->
    Pad = ?PAD_SIZE(Y, 8),
    <<?SKIP(Pad),X:64/little-float,Bin1/binary>> = Bin,
    ?TRACE("~w: double:~f\n", [Y, X]),
    {X, 8+Pad, Bin1};
decode_double(Bin, Y, big) ->
    Pad = ?PAD_SIZE(Y, 8),
    <<?SKIP(Pad),X:64/big-float,Bin1/binary>> = Bin,
    ?TRACE("~w: double:~f\n", [Y, X]),
    {X, 8+Pad, Bin1}.

decode_string(Bin, Y, Endian) ->
    {Size, LenSize, Bin1} = decode_len(Bin, Y, Endian),
    <<Data:Size/binary,0,Bin2/binary>> = Bin1,
    String = unicode:characters_to_list(Data),
    ?TRACE("~w: string:\"~ts\"\n", [Y, String]),
    {String,LenSize+Size+1,Bin2}.

decode_dict(KSig, VSig, Y, Endian, Bin) ->
    decode_dict_(KSig, VSig, Y, Endian, Bin, 0, []).

decode_dict_(_KSig, _VSig, _Y, _Endian,  <<>>, Len, Acc) ->
    {Acc, Len};
decode_dict_(KSig, VSig, Y, Endian, Bin, Len, Acc) ->
    EntPad = ?PAD_SIZE(Y,8),
    <<_:EntPad/binary, Bin0/binary>> = Bin,
    {K,KLen,Bin1} = decode(KSig,Bin0,Y+EntPad,Endian),
    {V,VLen,Bin2} = decode(VSig,Bin1,Y+EntPad+KLen,Endian),
    DLen = EntPad+KLen+VLen,
    decode_dict_(KSig,VSig,Y+DLen,Endian,Bin2,Len+DLen,[{K,V}|Acc]).

decode_array(Sig,Bin,Y,Endian) ->
    decode_array_(Sig,Bin,Y,Endian,0,[]).

decode_array_(_Sig,<<>>,_Y,_Endian,Len,Acc) ->
    {reverse(Acc),Len};
decode_array_(Sig,Bin,Y,Endian,Len,Acc) ->
    {X,XLen,Bin2} = decode(Sig,Bin,Y,Endian),
    decode_array_(Sig,Bin2,Y+XLen,Endian,Len+XLen,[X|Acc]).

decode_struct(SigList,Bin,Y,Endian) ->
    decode_struct_(SigList,Bin,Y,Endian,0,[]).

decode_struct_(SigList,Bin,Y,Endian,Len,Acc) ->
    case next_arg(SigList) of
	{")",""} ->
	    {list_to_tuple(reverse(Acc)),Len,Bin};
	{Sig,SigList1} ->
	    {E,ELen,Bin1} = decode(Sig,Bin,Y,Endian),
	    decode_struct_(SigList1,Bin1,Y+ELen,
			   Endian,Len+ELen,[E|Acc])
    end.

decode_seq(SigList,Bin,Y,Endian) ->
    decode_seq_(SigList,Bin,Y,Endian,0,[]).

decode_seq_("",Bin,_Y,_Endian,Len,Acc) ->
    {reverse(Acc),Len,Bin};
decode_seq_(SigList,Bin,Y,Endian,Len,Acc) ->
    {Sig,SigList1} = next_arg(SigList),
    {E,ELen,Bin1} = decode(Sig,Bin,Y,Endian),
    decode_seq_(SigList1,Bin1,Y+ELen,
		Endian,Len+ELen,[E|Acc]).

%%
%% @doc
%%   Fetch next argument in type spec.
%%   Examples:
%%   next_arg("ii") -> {"i", "i"}
%%   next_arg("a{i(ddd)}i") -> {"a{i(ddd)}", "i"}
%% @end
-spec next_arg(Sig::string()) -> {string(), string()}.

next_arg([?DBUS_STRUCT_BEGIN|Es]) ->
    next_arg(Es, [?DBUS_STRUCT_END], [?DBUS_STRUCT_BEGIN]);
next_arg([?DBUS_DICT_ENTRY_BEGIN|Es]) ->
    next_arg(Es, [?DBUS_DICT_ENTRY_END], [?DBUS_DICT_ENTRY_BEGIN]);
next_arg([?DBUS_ARRAY|Es]) ->
    {A,Es1} = next_arg(Es),
    {[?DBUS_ARRAY|A],Es1};
next_arg([E|Es]) ->
    {[E], Es}.

next_arg([E|Es], [E], Acc) ->
    {reverse([E|Acc]), Es};
next_arg([E|Es], [E|Fs], Acc) ->
    next_arg(Es, Fs, [E|Acc]);
next_arg([?DBUS_STRUCT_BEGIN|Es], Fs, Acc) ->
    next_arg(Es, [?DBUS_STRUCT_END|Fs], [?DBUS_STRUCT_BEGIN|Acc]);
next_arg([?DBUS_DICT_ENTRY_BEGIN|Es], Fs, Acc) ->
    next_arg(Es, [?DBUS_DICT_ENTRY_END|Fs], [?DBUS_DICT_ENTRY_BEGIN|Acc]);
next_arg([E|Es], Fs=[_|_], Acc) ->
    next_arg(Es, Fs, [E|Acc]).

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
	term    -> [?DBUS_ERLANG];
	_ -> []
    end.

signature_to_type_spec(Spec) ->
    case Spec of
	[?DBUS_BYTE]    -> byte;
	[?DBUS_BOOLEAN] -> boolean;
	[?DBUS_INT16]   -> int16;
	[?DBUS_UINT16]  -> uint16;
	[?DBUS_INT32]   -> int32;
	[?DBUS_UINT32]  -> uint32;
	[?DBUS_INT64]   -> int64;
	[?DBUS_UINT64]  -> uint64;
	[?DBUS_DOUBLE]  -> double;
	[?DBUS_UNIX_FD] -> unix_fd;
	[?DBUS_STRING]  -> string;
	[?DBUS_VARIANT] -> variant;
	[?DBUS_OBJPATH] -> objpath;
	[?DBUS_ERLANG]  -> term;
	[?DBUS_ARRAY,?DBUS_DICT_ENTRY_BEGIN|Es] ->
	    {K,V,""} = dict_to_type_spec(Es),
	    {dict,K,V};
	[?DBUS_ARRAY|Es] ->
	    {E,""} = next_arg(Es),  %% element type
	    {array, signature_to_type_spec(E)};
	[?DBUS_STRUCT_BEGIN|Es] ->
	    {As,""} = signature_struct_to_type_spec(Es,[]),
	    {struct, As}
    end.

signature_struct_to_type_spec([?DBUS_STRUCT_END|Es],Acc) ->
    {reverse(Acc),Es};
signature_struct_to_type_spec(Es,Acc) ->
    {E,Es1} = next_arg(Es),
    signature_struct_to_type_spec(Es1, [signature_to_type_spec(E)|Acc]).

dict_to_type_spec(Es) ->
    {K,Es1} = next_arg(Es),
    {V,[?DBUS_DICT_ENTRY_END|Es2]} = next_arg(Es1),
    {signature_to_type_spec(K),signature_to_type_spec(V),Es2}.

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

is_valid_bus_name(_Bus) ->
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

-ifdef(TEST).

%%
%% Testing
%%
perf() ->
    perf(10000).

perf(N) ->
    Sig = "(ya(nqiuxt)a{su}dsov)",
    V = {100,[{1,2,3,4,5,6},
	      {7,8,9,10,11,12},
	      {13,14,15,16,17,18},
	      {19,20,21,22,23,24}],
	 #{"a"=>1,"b"=>2,"c"=>3},
	 3.14,"hello","/foo/bar", {"s","test"}},
    Endian = big,
    perf_data(N, Endian, Sig, V).

perf_byte_array(N) ->
    Spec = "ay",  %% byte array
    V = lists:seq(0,255),
    Endian = little,
    perf_data(N, Endian, Spec, V).

perf_uint32_array(N) ->
    Spec = "au",  %% uint32 array
    V = lists:seq(0,1024),  %% 4K array
    Endian = little,
    perf_data(N, Endian, Spec, V).

perf_binary(N) ->
    Spec = "ay",  %% byte array
    V = iolist_to_binary(lists:seq(0,255)),
    Endian = little,
    perf_data(N, Endian, Spec, V).

perf_data(N, Endian, Spec, Value) ->
    test_type(Endian, Spec, Value),

    T0 = erlang:monotonic_time(),
    perf_encode(N, Endian, Spec, Value),
    T1 = erlang:monotonic_time(),

    {IOList,_Y1} = encode(Spec, Value, 0, Endian),
    Bin = iolist_to_binary(IOList),
    T2 = erlang:monotonic_time(),
    perf_decode(N, Endian, Spec, Bin),
    T3 = erlang:monotonic_time(),

    Te = erlang:convert_time_unit(T1-T0,native,microsecond),
    Td = erlang:convert_time_unit(T3-T2,native,microsecond),

    { (N / Te)*1000000,  (N / Td)*1000000 }.


perf_encode(0, _E, _Spec, _V) ->
    ok;
perf_encode(I, E, Spec, V) ->
    encode(Spec, V, 0, E),
    perf_encode(I-1,E,Spec,V).

perf_decode(0, _E, _Spec, _Bin) ->
    ok;
perf_decode(I, Endian, Spec, Bin) ->
    decode(Spec, Bin, 0, Endian),
    perf_decode(I-1,Endian,Spec,Bin).



test() ->
    test_basic(),
    test_struct(),
    test_array(),
    test_dict(),
    test_variant(),
    test_nested(),
    %% test_erlang(),
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
    test_type("(sid)", {"x",1,2.1}),
    test_type("(nqiuxt)", {1,2,3,4,5,6}),
    test_type("(nqiuxt)", {-10,10,-20,20,-30,30}).
    

test_array() ->
    test_type("ai", [1,2,3,4]),
    test_type("ad", [1.0,2.0,3.0,4.0]),
    test_type("a{is}", #{1=>"x",2=>"y",3=>"z"}),
    test_type("a(ibs)", [{1,true,"x"},{2,false,"y"}]).

test_dict() ->
    test_type("a{ii}", #{ 1=>1, 2=>2, 3=>3, 4=>4 }),
    test_type("a{is}", #{1=>"x",2=>"y",3=>"z"}),
    test_type("a{ii}", #{}),
    test_type("a{sd}", #{ "a"=>1.0,"b"=>2.0,"c"=>3.0,"d"=>4.0}).

test_variant() ->
    test_type("v", {"(iii)", {1,2,3}}),
    test_type("v", {"d", 2.0}),
    test_type("v", {"ai", [1,2,3,4]}).

test_nested() ->
    test_type("(ii)", {1,2}),
    test_type("((i))", {{1}}),
    test_type("a{sa{ii}}", #{ "a" => #{ 1=>1 }}).
%%    test_type("(a{si}a{is})", { #{ "a"=>1, "b"=>2 }, #{ 1=>"a", 2=>"b"} }).

test_erlang() ->
    test_type("E", {"hello",1.0,"world",{1,2,3}}),
    test_type("E", {#{"a"=>1,17=>1},#{"b"=>18,18=>2}}).


test_type(Spec, Value) ->
    test_type(little, Spec, Value),
    test_type(big, Spec, Value).

test_type(Endian, Spec, Value) ->
    io:format("test: ~w spec=~s, value=~p\n", [Endian, Spec, Value]),
    Y0 = 0,
    {IOList,_Len} = encode(Spec, Value, Y0, Endian),
    ?DBG("len:~w, size:~w, bin:~p\n", [_Len, iolist_size(IOList), IOList]),
    {Value1,_Y1,<<>>} = decode(Spec,iolist_to_binary(IOList),Y0,Endian),
    if is_binary(Value1), is_list(Value) ->
	    Value = binary_to_list(Value1);  %% special for testing "ay"
       Value1 =:= Value ->
	    true;
       true ->
	    io:format("bad_match:\n~p\ndo not match\n~p\n", 
		      [Value1, Value]),
	    error(bad_match)
    end.

-endif.
