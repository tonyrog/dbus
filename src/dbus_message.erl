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
%%% Created :  5 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(dbus_message).

-include("../include/dbus.hrl").

-compile(export_all).
-export([encode_call/5]).
-export([encode_return/5]).
-export([encode_signal/5]).
-export([encode_error/5]).
-export([decode/1]).

%% -define(debug(F,A), io:format((F),(A))).
-define(debug(F,A), ok).

-define(HEADER_SIGNATURE, "yyyyuua(yv)").


encode_call(Endian, Serial, Fields, Signature, Args) ->
    Fd = fields(Fields),
    E = get_endian(Endian),
    {Body,Length} = dbus_codec:encode_args(E,0,Signature,Args),
    Signature1 = dbus_codec:efilter(Signature),
    H = #dbus_header { endian=E,
		       message_type=method_call,
		       length=Length,
		       serial=Serial, 
		       fields = Fd#dbus_field { signature=Signature1} 
		     },
    {Header,Offs} = encode_dbus_header(H),
    Pad = ?PAD_SIZE(Offs,8),
    {[Header,<<?PAD(Pad)>>,Body],Offs+Pad+Length}.

encode_return(Endian, Serial, Fields, Signature, Args) ->
    Fd = fields(Fields),
    E = get_endian(Endian),
    {Body,Length} = dbus_codec:encode_args(E,0,Signature,Args),
    Signature1 = dbus_codec:efilter(Signature),
    Fd1 = Fd#dbus_field { signature = Signature1 },
    H = #dbus_header { endian=E, 
		       message_type=method_return,
		       flags = [no_reply_expected],
		       length=Length,
		       serial=Serial,
		       fields = Fd1
		     },
    ?debug("ENCODE_RAW_RETURN: H=~p, Body=~p\n", [H, Body]),
    {Header,Offs} = encode_dbus_header(H),
    Pad = ?PAD_SIZE(Offs,8),
    {[Header,<<?PAD(Pad)>>,Body],Offs+Pad+Length}.

encode_signal(Endian, Serial, Fields, Signature, Arguments) ->
    Fd = fields(Fields),
    E = get_endian(Endian),
    {Body,Length} = dbus_codec:encode_args(E,0,Signature,Arguments),
    Signature1 = dbus_codec:efilter(Signature),
    Fd1 = Fd#dbus_field { signature=Signature1 },
    H = #dbus_header { endian=E, 
		       message_type=signal,
		       length=Length, 
		       serial=Serial, 
		       fields = Fd1
		     },
    ?debug("ENCODE_SIGNAL: H=~p, Body=~p\n", [H, Body]),
    {Header,Offs} = encode_dbus_header(H),
    Pad = ?PAD_SIZE(Offs,8),
    {[Header,<<?PAD(Pad)>>,Body],Offs+Pad+Length}.

encode_error(Endian, Serial, Fields, ErrorName, ErrorText) ->
    Fd = fields(Fields),
    E = get_endian(Endian),
    Signature = "s",
    Arguments = [ErrorText],
    {Body,Length} = dbus_codec:encode_args(E,0,Signature,Arguments),
    H = #dbus_header { endian=E, 
		       message_type=error,
		       length=Length,
		       serial=Serial,
		       fields = Fd#dbus_field { signature=Signature,
						error_name=ErrorName }
		     },
    ?debug("ENCODE_ERROR: H=~p, Body=~p\n", [H, Body]),
    {Header,Offs} = encode_dbus_header(H),
    Pad = ?PAD_SIZE(Offs,8),
    {[Header,<<?PAD(Pad)>>,Body],Offs+Pad+Length}.

%%
%% Decode data 
%%
decode(Bin) ->
    {H,Offs,Bin1} = decode_dbus_header(0,Bin),
    Pad = ?PAD_SIZE(Offs,8),
    Length = H#dbus_header.length,
    <<?PAD(Pad),Body:Length/binary,_Pad/binary>> = Bin1,
    Fd = H#dbus_header.fields,
    Signature = Fd#dbus_field.signature,
    {Data,_Offs1,Bin2} =
	dbus_codec:decode_args(H#dbus_header.endian,0,Signature,Body),
    {{H,Data}, Bin2}.

%% Header signature "yyyyuua(yv)"
%% HeaderFields = [{FieldCode,{Signature,FieldValue}}]
%% Length = length of message body 
%% Serial = sequence number between client & server
%% Header is 8-byte aligned then message follows
encode_dbus_header(#dbus_header { endian=Endian0, message_type=MessageType,
				  flags=Flags, version=Version,
				  length=Length, serial=Serial,
				  fields=Fds }) ->
    Endian = get_endian(Endian0),
    dbus_codec:encode_args(Endian, 0,
			   ?HEADER_SIGNATURE,
			   [ encode_dbus_endian(Endian),
			     encode_dbus_message_type(MessageType),
			     encode_dbus_flags(Flags),
			     Version,
			     Length,
			     Serial,
			     make_dbus_fields(Fds)]).

decode_dbus_header(Y,Bin = <<$l,_/binary>>) ->
    decode_dbus_header_(little,Y,Bin);
decode_dbus_header(Y,Bin = <<$B,_/binary>>) ->
    decode_dbus_header_(big,Y,Bin);
decode_dbus_header(_Y,<<>>) ->
    erlang:error(more_data).


decode_dbus_header_(E,Y,Bin) when is_binary(Bin) ->
    {[_Endian,MessageType,Flags,Version,Length,Serial,Fields],Y1,Bin1} =
	dbus_codec:decode_args(E,Y,?HEADER_SIGNATURE,Bin),
    H = #dbus_header { endian = E,
		       message_type = decode_dbus_message_type(MessageType),
		       flags = decode_dbus_flags(Flags),
		       version = Version,
		       length = Length,
		       serial = Serial,
		       fields = decode_dbus_fields(Fields)
		     },
    { H, Y1, Bin1 }.

%%
%% Default engine
%%
get_endian(big)    -> big;
get_endian(little) -> little;
get_endian(native) -> erlang:system_info(endian).

%%
%% Endian names
%%
encode_dbus_endian(little) -> $l;
encode_dbus_endian(big) ->  $B.

decode_dbus_endian($l) -> little;
decode_dbus_endian($B) -> big.

%%
%% Message types
%%
encode_dbus_message_type(method_call) -> 1;
encode_dbus_message_type(method_return) -> 2;
encode_dbus_message_type(error) -> 3;
encode_dbus_message_type(signal) -> 4.

decode_dbus_message_type(1) -> method_call;
decode_dbus_message_type(2) -> method_return;
decode_dbus_message_type(3) -> error;
decode_dbus_message_type(4) -> signal.

%%
%% Flags
%%
encode_dbus_flags([no_reply_expected|Fs]) -> 
    16#01 bor encode_dbus_flags(Fs);
encode_dbus_flags([no_auto_start|Fs]) ->
    16#02 bor encode_dbus_flags(Fs);
encode_dbus_flags([allow_interactive_authorization|Fs]) ->
    16#04 bor encode_dbus_flags(Fs);
encode_dbus_flags([]) ->  0.

decode_dbus_flags(Flags) ->
    if Flags band 16#01 =/= 0 -> [no_reply_expected]; true -> [] end ++
	if Flags band 16#02 =/= 0 -> [no_auto_start]; true -> [] end ++
	if Flags band 16#04 =/= 0 ->  [allow_interactive_authorization]; true -> [] end.
		 
%%
%% Fields
%%
make_dbus_fields(Fds) ->
    lists:append([
		  make_dbus_field(path,         Fds#dbus_field.path),
		  make_dbus_field(interface,    Fds#dbus_field.interface),
		  make_dbus_field(member,       Fds#dbus_field.member),
		  make_dbus_field(error_name,   Fds#dbus_field.error_name),
		  make_dbus_field(reply_serial, Fds#dbus_field.reply_serial),
		  make_dbus_field(destination,  Fds#dbus_field.destination),
		  make_dbus_field(sender,       Fds#dbus_field.sender),
		  make_dbus_field(signature,    Fds#dbus_field.signature),
		  make_dbus_field(unix_fds,     Fds#dbus_field.unix_fds)]).

make_dbus_field(_Field, undefined) -> 
    [];
make_dbus_field(Field, Value) -> 
    {Code,Type} = dbus_field_variant(Field),
    [{Code,{Type,Value}}].

dbus_field_variant(path)         -> {1, [?DBUS_OBJPATH]};
dbus_field_variant(interface)    -> {2, [?DBUS_STRING]};
dbus_field_variant(member)       -> {3, [?DBUS_STRING]};
dbus_field_variant(error_name)   -> {4, [?DBUS_STRING]};
dbus_field_variant(reply_serial) -> {5, [?DBUS_UINT32]};
dbus_field_variant(destination)  -> {6, [?DBUS_STRING]};
dbus_field_variant(sender)       -> {7, [?DBUS_STRING]};
dbus_field_variant(signature)    -> {8, [?DBUS_SIGNATURE]};
dbus_field_variant(unix_fds)     -> {9, [?DBUS_UINT32]}.

decode_dbus_fields(Fs) ->
    decode_dbus_fields(Fs, #dbus_field{}).

decode_dbus_fields([{Field,Value}|Fs], F) ->
    case Field+1 of
	#dbus_field.path -> 
	    decode_dbus_fields(Fs, F#dbus_field { path=Value});
	#dbus_field.interface ->
	    decode_dbus_fields(Fs, F#dbus_field { interface=Value});
	#dbus_field.member ->
	    decode_dbus_fields(Fs, F#dbus_field { member=Value});
	#dbus_field.error_name ->
	    decode_dbus_fields(Fs, F#dbus_field { error_name=Value});
	#dbus_field.reply_serial ->
	    decode_dbus_fields(Fs, F#dbus_field { reply_serial=Value});
	#dbus_field.destination ->
	    decode_dbus_fields(Fs, F#dbus_field { destination=Value});
	#dbus_field.sender ->
	    decode_dbus_fields(Fs, F#dbus_field { sender=Value});
	#dbus_field.signature -> 
	    decode_dbus_fields(Fs, F#dbus_field { signature=Value});
	#dbus_field.unix_fds -> 
	    decode_dbus_fields(Fs, F#dbus_field { unix_fds=Value})
    end;
decode_dbus_fields([], F) ->
    F.

%% build fields record from properties
fields(Fs) when is_list(Fs) ->
    fields_(Fs, #dbus_field{});
fields(F) when is_record(F, dbus_field) ->
    F.

fields_([{Field,Value}|Fs], F) ->
    case Field of
	path -> 
	    fields_(Fs, F#dbus_field { path=Value});
	interface ->
	    fields_(Fs, F#dbus_field { interface=Value});
	member ->
	    fields_(Fs, F#dbus_field { member=Value});
	error_name ->
	    fields_(Fs, F#dbus_field { error_name=Value});
	reply_serial ->
	    fields_(Fs, F#dbus_field { reply_serial=Value});
	destination ->
	    fields_(Fs, F#dbus_field { destination=Value});
	sender ->
	    fields_(Fs, F#dbus_field { sender=Value});
	signature -> 
	    fields_(Fs, F#dbus_field { signature=Value});
	unix_fds -> 
	    fields_(Fs, F#dbus_field { unix_fds=Value})
    end;
fields_([], F) ->
    F.

