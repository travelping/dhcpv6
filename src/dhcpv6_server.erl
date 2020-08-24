%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_server).

%% API
%% -export([fmt_clientid/1, fmt_ip/1]).
-export([handle_dhcp/2]).

-include_lib("kernel/include/logger.hrl").
-include("dhcpv6.hrl").

-define(INADDR_ANY, {0, 0, 0, 0}).
-define(ALLOC_OPTS, [?D6O_IA_NA, ?D6O_IA_TA, ?D6O_IA_PD]).

%%%-------------------------------------------------------------------
%%% The DHCP message handler
%%%-------------------------------------------------------------------
handle_dhcp(#dhcpv6{op = ?DHCPV6_SOLICIT,
		    options =
			#{?D6O_CLIENTID := ClientId} = ReqOpts} = D,
	    Config) ->
    Opts = process_ia(fun dhcpv6_alloc:reserve/5, ClientId, ReqOpts),
    advertise(D, Opts, Config);

handle_dhcp(#dhcpv6{op = ?DHCPV6_REQUEST,
		    options =
			#{?D6O_SERVERID := ServerId,
			  ?D6O_CLIENTID := ClientId} = ReqOpts} = D,
	    #{server_id := ServerId} = Config) ->
    Opts = process_ia(fun dhcpv6_alloc:allocate/5, ClientId, ReqOpts),
    reply(D, Opts, Config);

handle_dhcp(#dhcpv6{op = ?DHCPV6_RENEW,
		    options =
			#{?D6O_SERVERID := ServerId,
			  ?D6O_CLIENTID := ClientId} = ReqOpts} = D,
	    #{server_id := ServerId} = Config) ->
    Opts = process_ia(fun dhcpv6_alloc:extend/5, ClientId, ReqOpts),
    reply(D, Opts, Config);

handle_dhcp(#dhcpv6{op = ?DHCPV6_REBIND,
		    options =
			#{?D6O_SERVERID := ServerId,
			  ?D6O_CLIENTID := ClientId} = ReqOpts} = D,
	    #{server_id := ServerId} = Config) ->
    Opts = process_ia(fun dhcpv6_alloc:extend/5, ClientId, ReqOpts),
    reply(D, Opts, Config);

handle_dhcp(#dhcpv6{op = ?DHCPV6_DECLINE,
		    options =
			#{?D6O_SERVERID := ServerId,
			  ?D6O_CLIENTID := ClientId} = ReqOpts},
	    #{server_id := ServerId}) ->
    process_ia(fun dhcpv6_alloc:decline/5, ClientId, ReqOpts);

handle_dhcp(#dhcpv6{op = ?DHCPV6_RELEASE,
		    options =
			#{?D6O_CLIENTID := ClientId} = ReqOpts} = D,
	    Config) ->
    Opts = process_ia(fun dhcpv6_alloc:release/5, ClientId, ReqOpts),
    reply(D, Opts, Config);

handle_dhcp(#dhcpv6{op = ?DHCPV6_INFORMATION_REQUEST,
		    options =
			#{?D6O_SERVERID := ServerId,
			  ?D6O_CLIENTID := ClientId} = ReqOpts} = D,
	    #{server_id := ServerId} = Config) ->
    Opts = process_ia(fun dhcpv6_alloc:local_conf/5, ClientId, ReqOpts),
    reply(D, Opts, Config);

handle_dhcp(#dhcpv6{op = Op}, _Config) ->
    ?LOG(error, "Invalid DHCPv6 message type ~p", [Op]),
    ok.

reply(Op, D, Opts, #{server_id := ServerId}) ->
    {reply, D#dhcpv6{op = Op, options = Opts#{?D6O_SERVERID => ServerId}}}.

advertise(D, Opts, Config) ->
    reply(?DHCPV6_ADVERTISE, D, Opts, Config).

reply(D, Opts, Config) ->
    reply(?DHCPV6_REPLY, D, Opts, Config).

append_code(Code, Value, Opts) ->
    UpdF = fun(M) -> maps:merge(M, Value) end,
    maps:update_with(Code, UpdF, Value, Opts).

encode_ia(?D6O_IA_PD, Id, _IdOpts, {IP, PrefixLen}, IAOpts, MOpts) ->
    IA = {IP, PrefixLen},
    PrefLife = 3600,
    ValidLife = 7200,
    PD = #{?D6O_IAPREFIX => #{IA => {PrefLife, ValidLife, maps:from_list(IAOpts)}}},
    T1 = 0,
    T2 = 0,
    append_code(?D6O_IA_PD, #{Id => {T1, T2, PD}}, MOpts);
encode_ia(_Code, _Id, _IdOpts, _IA, _IAOpts, MOpts) ->
    MOpts.

process_ia(Fun, ClientId, ReqOpts) ->
    Init = #{?D6O_CLIENTID => ClientId},
    lists:foldl(
      fun(Code, Opts) ->
	 maps:fold(
	   fun(Id, IdOpts, MOpts) ->
		   case Fun(ClientId, Code, Id, IdOpts, ReqOpts) of
		       {ok, IA, IAOpts} ->
			   encode_ia(Code, Id, IdOpts, IA, IAOpts, MOpts);
		       _ ->
			   MOpts
		   end
	   end, Opts, maps:get(Code, ReqOpts, #{}))
	 end, Init, ?ALLOC_OPTS).
