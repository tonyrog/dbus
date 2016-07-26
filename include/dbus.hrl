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
-ifndef(__DBUS_HRL__).
-define(__DBUS_HRL__, true).

-define(PAD(Bytes),  0:(Bytes)/integer-unit:8).
-define(SKIP(Bytes),  _:(Bytes)/integer-unit:8).

-define(PAD_SIZE(Y,N),
	(((N)-((Y) band ((N)-1))) band ((N)-1))).

-define(PAD_ADD(Y,N),
	(((Y)+((N)-1)) band bnot ((N)-1))).

-define(DBUS_BYTE,     $y).  %% 8-but unsigned integer
-define(DBUS_BOOLEAN,  $b).  %% 0 = FALSE, 1=TRUE only accepted values
-define(DBUS_INT16,    $n).
-define(DBUS_UINT16,   $q).
-define(DBUS_INT32,    $i).
-define(DBUS_UINT32,   $u).
-define(DBUS_INT64,    $x).
-define(DBUS_UINT64,   $t).
-define(DBUS_DOUBLE,   $d).  %% IEEE 754 double
-define(DBUS_STRING,   $s).  %% UTF-8 string (valid) nul terminated!
-define(DBUS_OBJPATH,  $o).  %% Name of object instance
-define(DBUS_SIGNATURE, $g).
-define(DBUS_ARRAY,  $a).      %% Array
-define(DBUS_STRUCT_BEGIN, 40).  %% (
-define(DBUS_STRUCT_END,   41).  %% )
-define(DBUS_VARIANT, $v).
%% dict entry restrictions
%% always on form:  a{<k><v>}
%% <k> is primitive non repeated key
%% <v> is any value 
-define(DBUS_DICT_ENTRY_BEGIN, 123).  %% {
-define(DBUS_DICT_ENTRY_END,   125).  %% }
-define(DBUS_UNIX_FD,  $h).     %% Unix file descriptor

%% reserved for using in bindings (not used in signatures ?)
-define(DBUS_STRUCT, $r).
-define(DBUS_DICT_ENTRY, $e).

%% special hack to handle erlang terms, it should never be sent over 
%% the wire
-define(DBUS_ERLANG,  $E).

-type uint16_t() :: 0..65535.
-type uint32_t() :: 0..4294967295.

%% match rule (undefined normally means match )
-record(dbus_rule,
	{
	  type      :: undefined | method_call | method_return | error | signal,
	  sender    :: undefined | string(),
	  interface :: undefined | string(),
	  member    :: undefined | string(),
	  path      :: undefined | string(),
	  path_namespace :: undefined | string(),
	  destination    :: undefined | string(),
	  args           :: [{integer(),string()}],
	  argspath       :: [{integer(),string()}],
	  arg0namespace  :: undefined | string(),
	  eavesdrop      :: undefined | boolean()
	}).

-record(dbus_field,
	{
	  path,
	  interface,
	  member,
	  error_name,
	  reply_serial,
	  destination,
	  sender,
	  signature,
	  unix_fds
	}).

-record(dbus_header,
	{
	  endian :: little | big,
	  message_type :: method_call | method_return | error | signal,
	  flags = [] :: [ no_reply_expected | no_auto_start ],
	  version = 1 :: byte(),
	  length :: uint32_t(),
	  serial :: uint32_t(),
	  fields :: #dbus_field {}
	}).


-endif.
