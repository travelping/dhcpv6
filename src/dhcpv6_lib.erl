%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_lib).

%% API
-export([decode/1, encode/1]).
-export([ip2bin/1, bin2ip/1]).

-include("dhcpv6.hrl").

%%====================================================================
%% API
%%====================================================================

decode(<<Op, Hops, LinkAddr:16/bytes, PeerAddr:16/bytes, Options/binary>>)
  when Op =:= ?DHCPV6_RELAY_FORW; Op =:= ?DHCPV6_RELAY_REPL ->
    #dhcpv6_relay{
       op        = Op,
       hops      = Hops,
       link_addr = bin2ip(LinkAddr),
       peer_addr = bin2ip(PeerAddr),
       options   = decode_opts(Options)
      };

decode(<<Op, Xid:24, Options/binary>>) ->
    #dhcpv6{
       op        = Op,
       xid       = Xid,
       options   = decode_opts(Options)
      }.

encode(#dhcpv6_relay{op = Op, hops = Hops, link_addr = LinkAddr,
		     peer_addr = PeerAddr, options = Options}) ->
    Iter = maps:iterator(Options),
    encode_opts(maps:next(Iter), <<Op, Hops,
				   (ip2bin(LinkAddr))/binary,
				   (ip2bin(PeerAddr))/binary>>);

encode(#dhcpv6{op = Op, xid = Xid, options = Options}) ->
    encode_opts(Options, <<Op, Xid:24>>).

%%====================================================================
%% Internal functions
%%====================================================================
bin2ip(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    {A, B, C, D, E, F, G, H}.

ip2bin({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>.

append_opt(Code, Key, Value, Opts) when is_map_key(Code, Opts) ->
    maps:update_with(Code, fun(M) -> M#{Key => Value} end, Opts);
append_opt(Code, Key, Value, Opts) ->
    Opts#{Code => #{Key => Value}}.

decode_opts(Options) ->
    decode_opts(Options, #{}).

decode_opts(<<>>, Opts) ->
    Opts;
decode_opts(<<Code:16, Len:16, Bin:Len/bytes, Next/binary>>, Opts) ->
    decode_opts(Next, decode_opt(Bin, Code, Opts)).

encode_opts(Opts, Bin) ->
    Iter = maps:iterator(Opts),
    encode_opts_iter(maps:next(Iter), Bin).

encode_opts_iter(none, Bin) ->
    Bin;
encode_opts_iter({K, V, Iter}, Bin) ->
    encode_opts_iter(maps:next(Iter), encode_opt(K, V, Bin)).

decode_duid(<<1:16, HwType:16, Time:32, LL/binary>>, Code, Opts) ->
    Opts#{Code => {1, HwType, Time, LL}};
decode_duid(<<2:16, Enterp:32, Id/binary>>, Code, Opts) ->
    Opts#{Code => {2, Enterp, Id}};
decode_duid(<<3:16, HwType:16, LL/binary>>, Code, Opts) ->
    Opts#{Code => {3, HwType, LL}};
decode_duid(<<4:16, UUID/binary>>, Code, Opts) ->
    Opts#{Code => {4, UUID}};
decode_duid(DUID, Code, Opts) ->
    Opts#{Code => DUID}.

encode_duid({1, HwType, Time, LL}) ->
    <<1:16, HwType:16, Time:32, LL/binary>>;
encode_duid({2, Enterp, Id}) ->
    <<2:16, Enterp:32, Id/binary>>;
encode_duid({3, HwType, LL}) ->
    <<3:16, HwType:16, LL/binary>>;
encode_duid({4, UUID}) ->
    <<4:16, UUID/binary>>;
encode_duid(Bin) when is_binary(Bin) ->
    Bin.

decode_opt(Value, ?D6O_CLIENTID, Opts) ->
    decode_duid(Value, ?D6O_CLIENTID, Opts);
decode_opt(Value, ?D6O_SERVERID, Opts) ->
    decode_duid(Value, ?D6O_SERVERID, Opts);
decode_opt(<<IAID:32, T1:32, T2:32, IANA/binary>>, ?D6O_IA_NA, Opts) ->
    append_opt(?D6O_IA_NA, IAID, {T1, T2, decode_opts(IANA)}, Opts);
decode_opt(<<IAID:32, IATA/binary>>, ?D6O_IA_TA, Opts) ->
    append_opt(?D6O_IA_TA, IAID, decode_opts(IATA), Opts);
decode_opt(<<Addr:16/bytes, PrefLife:32, ValidLife:32, AddrOpts/binary>>, ?D6O_IAADDR, Opts) ->
    append_opt(?D6O_IAADDR, bin2ip(Addr), {PrefLife, ValidLife, decode_opts(AddrOpts)}, Opts);
decode_opt(Value, ?D6O_ORO, Opts) ->
    Opts#{?D6O_ORO => [X || <<X:16>> <= Value]};
decode_opt(<<Pref:8>>, ?D6O_PREFERENCE, Opts) ->
    Opts#{?D6O_PREFERENCE => Pref};
decode_opt(<<Time:16>>, ?D6O_ELAPSED_TIME, Opts) ->
    Opts#{?D6O_ELAPSED_TIME => Time * 10};
%% decode_opt(Value, ?D6O_RELAY_MSG, Opts) ->
%%     Opts#{?D6O_RELAY_MSG => decode(Bin)};
decode_opt(<<Proto, Algo, RDM, Replay:64/bits, Info/binary>>, ?D6O_AUTH, Opts) ->
    Opts#{?D6O_AUTH => {Proto, Algo, RDM, Replay, Info}};
decode_opt(Addr, ?D6O_UNICAST, Opts)
  when byte_size(Addr) =:= 16 ->
    Opts#{?D6O_UNICAST => bin2ip(Addr)};
decode_opt(<<Code:16, Message/binary>>, ?D6O_STATUS_CODE, Opts) ->
    Opts#{?D6O_STATUS_CODE => {Code, Message}};
decode_opt(<<>>, ?D6O_RAPID_COMMIT, Opts) ->
    Opts#{?D6O_RAPID_COMMIT => true};
decode_opt(Value, ?D6O_USER_CLASS, Opts) ->
    Opts#{?D6O_USER_CLASS => [ Part || <<Len:16, Part:Len/bytes>> <= Value ]};
decode_opt(<<Vendor:32, Value/binary>>, ?D6O_VENDOR_CLASS, Opts) ->
    VClasses = [ Part || <<Len:16, Part:Len/bytes>> <= Value ],
    append_opt(?D6O_VENDOR_CLASS, Vendor, VClasses, Opts);
decode_opt(<<Vendor:32, Value/binary>>, ?D6O_VENDOR_OPTS, Opts) ->
    VOpts = [ {Code, Part} || <<Code:16, Len:16, Part:Len/bytes>> <= Value ],
    append_opt(?D6O_VENDOR_OPTS, Vendor, VOpts, Opts);
%% decode_opt(Value, ?D6O_INTERFACE_ID, Opts) ->
%%     Opts#{Code => Bin};
decode_opt(<<Type:8>>, ?D6O_RECONF_MSG, Opts) ->
    Opts#{?D6O_RECONF_MSG => Type};
decode_opt(<<>>, ?D6O_RECONF_ACCEPT, Opts) ->
    Opts#{?D6O_RECONF_ACCEPT => true};
decode_opt(<<IAID:32, T1:32, T2:32, IANA/binary>>, ?D6O_IA_PD, Opts) ->
    append_opt(?D6O_IA_PD, IAID, {T1, T2, decode_opts(IANA)}, Opts);
decode_opt(<<PrefLife:32, ValidLife:32, PLen:8, Prefix:16/bytes, PrefOpts/binary>>, ?D6O_IAPREFIX, Opts) ->
    append_opt(?D6O_IAPREFIX, {bin2ip(Prefix), PLen}, {PrefLife, ValidLife, decode_opts(PrefOpts)}, Opts);
decode_opt(<<Time:32>>, ?D6O_INFORMATION_REFRESH_TIME, Opts) ->
    Opts#{?D6O_INFORMATION_REFRESH_TIME => Time};
decode_opt(<<SolMaxRT:32>>, ?D6O_SOL_MAX_RT, Opts) ->
    Opts#{?D6O_SOL_MAX_RT => SolMaxRT};
decode_opt(<<InfMaxRT:32>>, ?D6O_INF_MAX_RT, Opts) ->
    Opts#{?D6O_INF_MAX_RT => InfMaxRT};
decode_opt(Servers, ?D6O_NAME_SERVERS, Opts) ->
     Opts#{?D6O_NAME_SERVERS => [ bin2ip(X) || <<X:128/bits>> <= Servers ]};
decode_opt(Code, Bin, Opts) ->
    Opts#{Code => Bin}.

encode_opt(?D6O_CLIENTID = Code, Value, Bin) ->
    encode_put(Code, encode_duid(Value), Bin);
encode_opt(?D6O_SERVERID = Code, Value, Bin) ->
    encode_put(Code, encode_duid(Value), Bin);
encode_opt(?D6O_IA_NA = Code, Value, Bin)
  when is_map(Value) ->
    maps:fold(
      fun(IAID, {T1, T2, Opts}, B) ->
	      encode_put(Code, encode_opts(Opts, <<IAID:32, T1:32, T2:32>>), B)
      end, Bin, Value);
encode_opt(?D6O_IA_TA = Code, Value, Bin)
  when is_map(Value) ->
    maps:fold(
      fun(IAID, Opts, B) ->
	      encode_put(Code, encode_opts(Opts, <<IAID:32>>), B)
      end, Bin, Value);
encode_opt(?D6O_IAADDR = Code, Value, Bin)
  when is_map(Value) ->
    maps:fold(
      fun(Addr, {PrefLife, ValidLife, Opts}, B) ->
	      IAAddr = <<(ip2bin(Addr)):16/bytes, PrefLife:32, ValidLife:32>>,
	      AddrBin = encode_opts(Opts, IAAddr),
	      encode_put(Code, AddrBin, B)
      end, Bin, Value);
encode_opt(?D6O_ORO = Code, Value, Bin)
  when is_list(Value) ->
       encode_put(Code, << <<X:16>> || X <- Value >>, Bin);
encode_opt(?D6O_PREFERENCE = Code, Pref, Bin) ->
    encode_put(Code, <<Pref:8>>, Bin);
encode_opt(?D6O_ELAPSED_TIME = Code, Time, Bin) ->
    encode_put(Code, <<(Time div 10):16>>, Bin);
%% encode_opt(?D6O_RELAY_MSG = Code, Value, Bin) ->
%%    encode_put(Code, encode(Value), Bin);
encode_opt(?D6O_AUTH = Code, {Proto, Algo, RDM, Replay, Info}, Bin) ->
    encode_put(Code, <<Proto, Algo, RDM, Replay:64/bits, Info/binary>>, Bin);
encode_opt(?D6O_UNICAST = Code, Value, Bin) ->
    encode_put(Code, ip2bin(Value), Bin);
encode_opt(?D6O_STATUS_CODE = Code, {MsgCode, Message}, Bin) ->
    encode_put(Code, <<MsgCode:16, Message/binary>>, Bin);
encode_opt(?D6O_RAPID_COMMIT = Code, true, Bin) ->
    encode_put(Code, <<>>, Bin);
encode_opt(?D6O_RAPID_COMMIT, _, Bin) ->
    Bin;
encode_opt(?D6O_USER_CLASS = Code, Value, Bin)
  when is_list(Value) ->
    encode_put(Code, << <<(size(Part)):16, Part/binary>> || Part <- Value>>, Bin);
encode_opt(?D6O_VENDOR_CLASS = Code, Value, Bin)
  when is_map(Value) ->
    maps:fold(
      fun(Vendor, VClasses, B) ->
	      VCBin =
		  << <<(size(Part)):16, Part/binary>> || Part <- VClasses >>,
	      encode_put(Code, <<Vendor:32, VCBin/binary>>, B)
      end, Bin, Value);
%% encode_opt(?D6O_INTERFACE_ID = Code, Value, Bin) ->
%%    encode_put(Code, Value, Bin);
encode_opt(?D6O_RECONF_MSG = Code, Type, Bin) ->
    encode_put(Code, <<Type:8>>, Bin);
encode_opt(?D6O_RECONF_ACCEPT = Code, true, Bin) ->
    encode_put(Code, <<>>, Bin);
encode_opt(?D6O_IA_PD = Code, Value, Bin)
  when is_map(Value) ->
    maps:fold(
      fun(IAID, {T1, T2, Opts}, B) ->
	      encode_put(Code, encode_opts(Opts, <<IAID:32, T1:32, T2:32>>), B)
      end, Bin, Value);
encode_opt(?D6O_IAPREFIX = Code, Value, Bin)
  when is_map(Value) ->
    maps:fold(
      fun({Prefix, PLen}, {PrefLife, ValidLife, Opts}, B) ->
	      IAPref = <<PrefLife:32, ValidLife:32, PLen:8, (ip2bin(Prefix))/binary>>,
	      CBin = encode_opts(Opts, IAPref),
	      encode_put(Code, CBin, B)
      end, Bin, Value);
encode_opt(?D6O_INFORMATION_REFRESH_TIME = Code, Time, Bin) ->
    encode_put(Code, <<Time:32>>, Bin);
encode_opt(?D6O_SOL_MAX_RT = Code, SolMaxRT, Bin) ->
    encode_put(Code, <<SolMaxRT:32>>, Bin);
encode_opt(?D6O_INF_MAX_RT = Code, InfMaxRT, Bin) ->
    encode_put(Code, <<InfMaxRT:32>>, Bin);
encode_opt(?D6O_NAME_SERVERS = Code, Servers, Bin) ->
    encode_put(Code, << <<(ip2bin(X))/binary>> || X <- Servers >>, Bin);
encode_opt(Code, Value, Bin) when is_binary(Value) ->
    encode_put(Code, Value, Bin).

encode_put(Code, Value, Bin) ->
    <<Bin/binary, Code:16, (size(Value)):16, Value/binary>>.
