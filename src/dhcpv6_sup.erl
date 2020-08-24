%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([get_config/0]).
%% Supervisor callbacks
-export([init/1]).

-import(lists, [keysearch/3, filter/2]).

-include("dhcpv6.hrl").
-include("dhcpv6_alloc.hrl").

-define(SERVER, ?MODULE).
-define(DHCP_LEASEFILE, "/var/run/dhcpv6_leases.dets").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    case get_config() of
	{ok, SocketOpts, ServerId, NextServer, LeaseFile, Subnets, Hosts} ->
	    io:format("SubNets: ~p~n", [Subnets]),
	    DHCPServer = {dhcpv6_udp_server, {dhcpv6_udp_server, start_link,
					    [SocketOpts, ServerId, NextServer]},
			  permanent, 2000, worker, [dhcpv6_server]},
	    %% DHCPServer = {dhcpv6_raw_server, {dhcpv6_raw_server, start_link,
	    %%				    [ServerId, NextServer]},
	    %%               permanent, 2000, worker, [dhcpv6_raw_server]},
	    DHCPAlloc = {dhcpv6_alloc, {dhcpv6_alloc, start_link,
				      [LeaseFile, Subnets, Hosts]},
			 permanent, 2000, worker, [dhcpv6_alloc]},
	    {ok, {{one_for_one, 0, 1}, [DHCPServer, DHCPAlloc]}};
	{error, Reason} ->
	    {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get_config() ->
    Config = application:get_all_env(),
    io:format("Config: ~p~n", [Config]),
    case process_config(Config) of
	{error, _} ->
	    get_config_file();
	Other ->
	    Other
    end.

get_config_file() ->
    ConfDir = case code:priv_dir(dhcp) of
		  PrivDir when is_list(PrivDir) -> PrivDir;
		  {error, _Reason} -> "."
	      end,
    case file:consult(filename:join(ConfDir, "dhcp.conf")) of
	{ok, Terms} ->
	    process_config(Terms);
	{error, Reason} ->
	    {error, Reason}
    end.

process_config(Config) ->
    case lists:keyfind(subnets, 1, Config) of
	false ->
	    {error, no_subnet_declaration};
	_ ->
	    SocketOpts =   proplists:get_value(socket,      Config),
	    ServerId =     proplists:get_value(server_id,   Config, ?ZERO_IPv6),
	    NextServer =   proplists:get_value(next_server, Config, ?ZERO_IPv6),
	    LeaseFile =    proplists:get_value(lease_file,  Config, ?DHCP_LEASEFILE),
	    Subnets =      [X || X <- proplists:get_value(subnets, Config, []), is_map(X)],
	    Hosts =        [X || X <- proplists:get_value(hosts,   Config, []), is_record(X, host)],
	    {ok, SocketOpts, ServerId, NextServer, LeaseFile, Subnets, Hosts}
    end.
