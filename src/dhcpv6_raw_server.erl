%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_raw_server).

-behaviour(gen_server).

%% API
-export([start_link/3, handle_dhcp/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include("dhcpv6.hrl").

-define(SERVER, ?MODULE).
-define(DHCP_RAW_PORT, 6767).
-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 68).
-define(INADDR_ANY, {0, 0, 0, 0}).
-define(INADDR_BROADCAST, {255, 255, 255, 255}).

-record(state, {config}).

-define(is_broadcast(D), (is_record(D, dhcp) andalso (D#dhcp.flags bsr 15) == 1)).

%%====================================================================
%% API
%%====================================================================

start_link(ServerId, NextServer, Session) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [ServerId, NextServer, Session], []).

handle_dhcp(Packet) when is_binary(Packet) ->
    gen_server:call(?SERVER, {dhcp, Packet}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ServerId, NextServer, Session]) ->
    ?LOG(info, "Starting DHCP server..."),
    {ok, #state{config = [#{server_id   => ServerId,
			    next_server => NextServer,
			    session     => Session}]}}.

handle_call({dhcp, <<_IhlVer:8/integer, _Tos:8/integer, _TotLen:16/integer,
		     _Id:16/integer, _FragOff:16/integer, _Ttl:8/integer, _Proto:8/integer,
		     _IPCsum:16/integer, SrcIP:4/bytes, _DstIP:4/bytes,
		     SrcPort:16/integer, _DstPort:16/integer, _UDPLen:16/integer, _UDPCsum:16/integer,
		     Packet/binary>>}, From, #state{config = Config} = State) ->
    Source = {SrcIP, SrcPort},
    try
	Request = dhcpv6_lib:decode(Packet),
	?LOG(debug, "DHCP Request: ~p, ~p", [Source, Request]),
	case dhcpv6_server:handle_dhcp(Request, Config) of
	    ok ->
		ok;
	    {reply, Reply} ->
		send_reply(From, Source, Reply);
	    {error, Reason} ->
		?LOG(error, Reason);
	    Other ->
		?LOG(debug, "DHCP result: ~w", [Other])
	end
    catch
	Class:Error ->
	    ?LOG(debug, "DHCP Request Decode failed with: ~p, ~p", [Class, Error]),
	    ok
    end,
    {noreply, State};
handle_call(_Request, _From, State) ->
    ?LOG(error, "unknown call: ~p", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_reply(From, {SrcIP, SrcPort}, Reply) ->
    ?LOG(debug, "Sending DHCP Reply to: ~w:~w", [SrcIP, SrcPort]),
    gen_server:reply(From, {reply, SrcIP, SrcPort, dhcpv6_lib:encode(Reply)}).
