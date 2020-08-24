%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_udp_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include("dhcpv6.hrl").

-define(SERVER, ?MODULE).

-record(state, {socket, config, burst_size = 1}).

%%====================================================================
%% API
%%====================================================================

start_link(SocketOpts, ServerId, NextServer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [SocketOpts, ServerId, NextServer], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([SocketOpts, ServerId, NextServer]) ->
    case make_socket(SocketOpts) of
	{ok, Socket} ->
	    ?LOG(info, "Starting DHCP server..."),
	    self() ! {'$socket', Socket, select, undefined},
	    {ok, #state{socket = Socket,
			config = #{server_id   => ServerId,
				   next_server => NextServer}}};
	{error, Reason} ->
	    ?LOG(error, "Cannot open udp port ~w",
				   [?DHCPv6_SERVER_PORT]),
	    {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'$socket', Socket, select, Info}, #state{socket = Socket} = State) ->
    handle_input(Socket, Info, State);

handle_info({'$socket', Socket, abort, Info}, #state{socket = Socket} = State) ->
    handle_input(Socket, Info, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_input(Socket, Info, #state{burst_size = BurstSize} = State) ->
    handle_input(Socket, Info, BurstSize, State).

handle_input(Socket, _Info, 0, State0) ->
    %% break the loop and restart
    self() ! {'$socket', Socket, select, undefined},
    {noreply, State0};

handle_input(Socket, Info, Cnt, State0) ->
    case socket:recvfrom(Socket, 0, [], nowait) of
	{error, _} ->
	    %% State = handle_err_input(Socket, State0),
	    handle_input(Socket, Info, Cnt - 1, State0);

	{ok, {Source, Data}} ->
	    State = handle_message(Source, Data, State0),
	    handle_input(Socket, Info, Cnt - 1, State);

	{select, _SelectInfo} ->
	    {noreply, State0}
    end.

handle_message(Source, Packet, #state{config = Config} = State) ->
    try
	Request = dhcpv6_lib:decode(Packet),
	?LOG(debug, "DHCP Request: ~p, ~p", [Source, Request]),
	case (catch dhcpv6_server:handle_dhcp(Request, Config)) of
	    ok ->
		ok;
	    {reply, Reply} ->
		send_reply(Source, Reply, State);
	    {error, Reason} ->
		?LOG(error, Reason);
	    Other ->
		?LOG(debug, "DHCP result: ~p", [Other])
	end
    catch
	Class:Error ->
	    ?LOG(debug, "DHCP Request Decode failed with: ~p, ~p", [Class, Error]),
	    ok
    end,
    State.

send_reply(Destination, Reply, #state{socket = Socket}) ->
    ?LOG(debug, "Sending DHCP Reply to: ~s", [fmt_addr(Destination)]),
    socket:sendto(Socket, dhcpv6_lib:encode(Reply), Destination, nowait).

%%%===================================================================
%%% Socket Helper
%%%===================================================================

fmt_addr(#{family := inet6, port := Port, addr := Addr}) when is_tuple(Addr) ->
    io_lib:format("~s:~w", [inet:ntoa(Addr), Port]);
fmt_addr(#{family := inet6, port := Port, addr := Addr}) when is_atom(Addr) ->
    io_lib:format("~s:~w", [Addr, Port]).

make_socket(#{netns := NetNs} = Opts) when is_list(NetNs) ->
    make_socket(#{netns => NetNs}, Opts);
make_socket(Opts) ->
    make_socket(#{}, Opts).

make_socket(Opts, SocketOpts) ->
    try socket:open(inet6, dgram, udp, Opts) of
	{ok, Socket} ->
	    bind_socket(Socket, SocketOpts);
	Other ->
	    Other
    catch
	_:Error ->
	    {error, Error}
    end.

make_addr(#{addr := Addr}) ->
    Addr;
make_addr(#{ip := IP}) ->
    #{family => inet6, addr => IP, port => ?DHCPv6_SERVER_PORT};
make_addr(_) ->
    #{family => inet6, addr => any, port => ?DHCPv6_SERVER_PORT}.

bind_socket(Socket, Opts) ->
    ok = socket:setopt(Socket, ipv6, v6only, true),
    ok = socket_netdev(Socket, Opts),
    {ok, _} = socket:bind(Socket, make_addr(Opts)),
    ok = socket:setopt(Socket, ipv6, recverr, true),
    ok = socket:setopt(Socket, ipv6, mtu_discover, dont),
    maps:fold(fun(K, V, ok) -> ok = socket_setopts(Socket, K, V) end, ok, Opts),
    {ok, Socket}.

%% socket_ip_freebind(Socket, #{freebind := true}) ->
%%     socket:setopt(Socket, ip, freebind, true);
%% socket_ip_freebind(_, _) ->
%%     ok.

socket_netdev(Socket, #{netdev := Device}) ->
    socket:setopt(Socket, socket, bindtodevice, Device);
socket_netdev(_, _) ->
    ok.

socket_join(Socket, IfIdx, local) ->
    ok = socket:setopt(Socket, ipv6, add_membership,
		       #{multiaddr => ?DHCPv6_NODE_LOCAL_ALL_ROUTERS, interface => IfIdx});
socket_join(Socket, IfIdx, all_dhcp_servers) ->
    ok = socket:setopt(Socket, ipv6, add_membership,
		       #{multiaddr => ?ALL_DHCPv6_SERVERS, interface => IfIdx});
socket_join(Socket, IfIdx, all_dhcp_relay_agents_and_servers) ->
    ok = socket:setopt(Socket, ipv6, add_membership,
		       #{multiaddr => ?ALL_DHCPv6_RELAY_AGENTS_AND_SERVERS, interface => IfIdx}).

socket_setopts(Socket, rcvbuf, Size) when is_integer(Size) ->
    case socket:setopt(Socket, socket, rcvbufforce, Size) of
	ok -> ok;
	_  -> socket:setopt(Socket, socket, rcvbuf, Size)
    end;
socket_setopts(Socket, reuseaddr, true) ->
    ok = socket:setopt(Socket, socket, reuseaddr, true);
socket_setopts(Socket, join, {IfIdx, Join}) when is_list(Join) ->
    [ok = socket_join(Socket, IfIdx, V) || V <- Join],
    ok;
socket_setopts(_Socket, _, _) ->
    ok.
