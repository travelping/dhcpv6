%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_alloc).

-behaviour(gen_server).

%% API
-export([start_link/3, reserve/5, allocate/5, release/5, verify/5,
	 extend/5, decline/5, local_conf/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("dhcpv6.hrl").
-include("dhcpv6_alloc.hrl").

-define(SERVER, ?MODULE).
-define(ADDRESS, dhcpv6_address).
-define(LEASE, dhcpv6_lease).

-define(DHCPOFFER_TIMEOUT, 3*60*1000).

-define(IS_ALLOCATED(A), A#address.status == allocated).
-define(IS_OFFERED(A), A#address.status == offered).
-define(IS_AVAILABLE(A), A#address.status == available).
-define(IS_DECLINED(A), A#address.status == declined).

-define(IS_NOT_EXPIRED(L, Now), L#lease.expires > Now).

-record(state, {subnets, hosts}).

%%====================================================================
%% API
%%====================================================================

start_link(LeaseFile, Subnets, Hosts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [LeaseFile, Subnets, Hosts], []).

reserve(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {reserve, ClientId, Type, Id, Opts, Request}).

allocate(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {allocate, ClientId, Type, Id, Opts, Request}).

release(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {release, ClientId, Type, Id, Opts, Request}).

verify(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {verify, ClientId, Type, Id, Opts, Request}).

extend(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {extend, ClientId, Type, Id, Opts, Request}).

local_conf(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {local_conf, ClientId, Type, Id, Opts, Request}).

decline(ClientId, Type, Id, Opts, Request) ->
    gen_server:call(?SERVER, {decline, ClientId, Type, Id, Opts, Request}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([LeaseFile, Subnets0, Hosts]) ->
    ets:new(?ADDRESS, [named_table, public, {keypos, #address.ip}]),
    dets:open_file(?LEASE, [{keypos, #lease.clientid}, {file, LeaseFile}]),
    Subnets = lists:map(fun init_subnet/1, Subnets0),
    {ok, #state{subnets = Subnets, hosts = Hosts}}.

handle_call({reserve, ClientId, Type, Id, Opts, _Request}, _From, State) ->
    IP = ip(Type, Id, Opts),
    Gateway = ?ZERO_IPv6,
    case select_subnet(Gateway, State#state.subnets) of
	{ok, Subnet} ->
	    case select_address(ClientId, IP, Subnet) of
		{ok, Address} ->
		    Reserved = reserve_address(Address, ClientId),
		    {reply, Reserved, State};
		{error, Reason} ->
		    {reply, {error, Reason}, State}
	    end;
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({allocate, ClientId, Type, Id, Opts, _Request}, _From, State) ->
    IP = ip(Type, Id, Opts),
    case ets:lookup(?ADDRESS, IP) of
	[A] when ?IS_OFFERED(A) ->
	    {reply, allocate_address(A, ClientId), State};
	_ ->
	    {reply, {error, "Address is not offered."}, State}
    end;
handle_call({release, ClientId, Type, Id, Opts, _Request}, _From, State) ->
    IP = ip(Type, Id, Opts),
    {reply, release_address(ClientId, IP), State};
handle_call({verify, ClientId, Type, Id, Opts, _Request}, _From, #state{subnets = SubNets} = State) ->
    IP = ip(Type, Id, Opts),
    Gateway = ?ZERO_IPv6,
    case select_subnet(Gateway, SubNets) of
	{ok, Subnet} ->
	    {reply, verify_address(ClientId, IP, Subnet), State};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({extend, ClientId, Type, Id, Opts, _Request}, _From, State) ->
    IP = ip(Type, Id, Opts),
    case ets:lookup(?ADDRESS, IP) of
	[A] when ?IS_ALLOCATED(A) ->
	    {reply, allocate_address(A, ClientId), State};
	_ ->
	    {reply, {error, "Address is not allocated."}, State}
    end;
handle_call({local_conf, _ClientId, _Type, _Id, Opts, _Request}, _From, #state{subnets = SubNets} = State) ->
    Gateway = ?ZERO_IPv6,
    case select_subnet(Gateway, SubNets) of
	{ok, #{options := Opts}} ->
	    {reply, {ok, Opts}, State};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({decline, ClientId, Type, Id, Opts, _Request}, _From, State) ->
    IP = ip(Type, Id, Opts),
    case ets:lookup(?ADDRESS, IP) of
	[A] when ?IS_ALLOCATED(A) ->
	    {reply, decline_address(A, ClientId), State};
	_ ->
	    {reply, {error, "Address not allocated."}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({expired, _ClientId, IP}, State) ->
    Address = #address{ip = IP, status = available},
    ets:insert(?ADDRESS, Address),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

addr({<<IP:128>>, PrefixLen}) ->
    {IP, PrefixLen};
addr({IP, PrefixLen}) when is_tuple(IP) ->
    {ip2int(IP), PrefixLen};
addr({IP, _} = Addr) when is_integer(IP) ->
    Addr.

ip(?D6O_IA_NA, _, {_, _, #{?D6O_IAADDR := Opts}}) ->
    addr({hd(maps:keys(Opts)), 128});
ip(?D6O_IA_NA, _, {_, _, _Opts}) ->
    {0, 128};

ip(?D6O_IA_TA, _, {_, _, #{?D6O_IAADDR := Opts}}) ->
    addr({hd(maps:keys(Opts)), 128});
ip(?D6O_IA_TA, _ , _Opts) ->
    {0, 128};

ip(?D6O_IA_PD, _, {_T1, _T2, #{?D6O_IAPREFIX := Prefix}}) ->
    addr(hd(maps:keys(Prefix)));
ip(?D6O_IA_PD, _, {_T1, _T2, _Opts}) ->
    {0, 0}.

select_subnet({IP, _}, SubNets) ->
    select_subnet(IP, SubNets);
select_subnet(_, []) ->
    {error, "No subnet."};
select_subnet(?ZERO_IPv6, [First | _]) ->
    {ok, First};
select_subnet(IP, [S|T]) ->
    case belongs_to_subnet(IP, S) of
	true ->
	    {ok, S};
	false ->
	    select_subnet(IP, T)
    end.

belongs_to_subnet({IP, _}, State) ->
    belongs_to_subnet(IP, State);
belongs_to_subnet(IP, #{subnet := {SubNet, PrefixLen}}) ->
    ip2int(SubNet) bsr (128 - PrefixLen) =:=
	ip2int(IP) bsr (128 - PrefixLen).

ip2int(IP) when is_integer(IP) ->
    IP;
ip2int(IP) when is_tuple(IP) ->
    binary:decode_unsigned(dhcpv6_lib:ip2bin(IP)).

int2ip({Int, PrefLen}) ->
    {int2ip(Int), PrefLen};
int2ip(Int) ->
    dhcpv6_lib:bin2ip(<<Int:128>>).

select_address(_ClientId, {0, 128}, #{subnet := SubNet, options := Opts}) ->
    F = fun(#address{subnet = SN} = A, false)
	      when ?IS_AVAILABLE(A) andalso SN =:= SubNet ->
		A;
	   (_, Acc) -> Acc
	end,
    case ets:foldl(F, false, ?ADDRESS) of
	false -> {error, "No available addresses in this subnet."};
	A     -> {ok, A#address{options = Opts}}
    end;
select_address(_ClientId, {0, PrefixLen}, #{subnet := SubNet, options := Opts}) ->
    F = fun(#address{ip = {_, PDLen}, subnet = SN} = A, false)
	      when ?IS_AVAILABLE(A) andalso
		   PDLen =/= 128 andalso (PDLen =:= PrefixLen orelse PrefixLen =:= 0) andalso
		   SN =:= SubNet ->
		A;
	   (_, Acc) ->
		Acc
	end,
    case ets:foldl(F, false, ?ADDRESS) of
	false -> {error, "No available addresses in this subnet."};
	A     -> {ok, A#address{options = Opts}}
    end;
select_address(ClientId, IP, #{options := Opts} = S) ->
    Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
    case belongs_to_subnet(IP, S) of
	true ->
	    case dets:lookup(?LEASE, ClientId) of
		[#lease{ip = IP} = L] when ?IS_NOT_EXPIRED(L, Now) ->
		    {ok, #address{ip = IP, options = Opts}};
		[#lease{ip = IP}] ->
		    case ets:lookup(?ADDRESS, IP) of
			[A] when ?IS_AVAILABLE(A) ->
			    {ok, A#address{options = Opts}};
			_ ->
			    dets:delete(?LEASE, ClientId),
			    select_address(ClientId, IP, S)
		    end;
		[] ->
		    case ets:lookup(?ADDRESS, IP) of
			[A] when ?IS_AVAILABLE(A) ->
			    {ok, A#address{options = Opts}};
			[A] when ?IS_OFFERED(A) ->
			    {error, "Already offered"};
			_ ->
			    select_address(ClientId, 0, S)
		    end
	    end;
	false ->
	    select_address(ClientId, 0, S)
    end.

verify_address(ClientId, IP, #{options := Opts} = S) ->
    case belongs_to_subnet(IP, S) of
	true ->
	    DateTime = {date(), time()},
	    Now = calendar:datetime_to_gregorian_seconds(DateTime),
	    case dets:lookup(?LEASE, ClientId) of
		[#lease{ip = IP} = L] when ?IS_NOT_EXPIRED(L, Now) ->
		    allocate_address(ClientId, IP, Opts),
		    {ok, IP, Opts};
		[_] ->
		    {error, "Address is not currently allocated to lease."};
		[] ->
		    nolease
	    end;
	false ->
	    release_address(ClientId, IP),
	    {error, "Wrong network."}
    end.

reserve_address(#address{ip = IP, options = Opts, timer = Timer} = A, ClientId) ->
    cancel_timer(Timer),
    T = erlang:send_after(?DHCPOFFER_TIMEOUT, ?SERVER,
			      {expired, ClientId, IP}),
    ets:insert(?ADDRESS, A#address{status = offered, timer = T}),
    DateTime = {date(), time()},
    Now = calendar:datetime_to_gregorian_seconds(DateTime),
    Expires = Now + ?DHCPOFFER_TIMEOUT,
    Lease = #lease{clientid = ClientId, ip = IP, expires = Expires},
    dets:insert(?LEASE, Lease),
    {ok, int2ip(IP), Opts}.

allocate_address(ClientId, IP, Options) ->
    allocate_address(#address{ip = IP, options = Options}, ClientId).

allocate_address(#address{ip = IP, options = Opts, timer = Timer} = A, ClientId) ->
    cancel_timer(Timer),
    {_Preferred, Valid} = lease_time(Opts),
    DateTime = {date(), time()},
    Gregorian = calendar:datetime_to_gregorian_seconds(DateTime),
    Expires = Gregorian + Valid,
    Lease = #lease{clientid = ClientId, ip = IP, expires = Expires},
    dets:insert(?LEASE, Lease),
    T = erlang:send_after(Valid * 1000, ?SERVER, {expired, ClientId, IP}),
    ets:insert(?ADDRESS, A#address{status = allocated, timer = T}),
    {ok, int2ip(IP), Opts}.

release_address(ClientId, IP) ->
    case ets:lookup(?ADDRESS, IP) of
	[#address{ip = IP, timer = Timer} = A] when ?IS_ALLOCATED(A) ->
	    case dets:lookup(?LEASE, ClientId) of
		[#lease{ip = IP} = L] ->
		    cancel_timer(Timer),
		    Address = #address{ip = IP, status = available},
		    ets:insert(?ADDRESS, Address),
		    DateTime = {date(), time()},
		    Now = calendar:datetime_to_gregorian_seconds(DateTime),
		    dets:insert(?LEASE, L#lease{expires = Now}),
		    ok;
		_ ->
		    {error, "Allocated to someone else."}
	    end;
	_ ->
	    {error, "Address not allocated."}
    end.

decline_address(#address{ip = IP, timer = Timer}, ClientId) ->
    case dets:lookup(?LEASE, ClientId) of
	[#lease{ip = IP, clientid = ClientId}] ->
	    cancel_timer(Timer),
	    ets:insert(?ADDRESS, #address{ip = IP, status = declined}),
	    dets:delete(?LEASE, ClientId),
	    ok;
	_ ->
	    {error, "Allocated to other lease."}
    end.

cancel_timer(Ref) when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive {timeout, Ref, _} -> 0
	    after 0 -> false
	    end;
	RemainingTime ->
	    RemainingTime
    end;
cancel_timer(_Timer) ->
    ok.

init_subnet(#{subnet := SN0} = SubNet0) ->
    SN = addr(SN0),
    SubNet = SubNet0#{subnet := SN},
    lists:foreach(fun(X) -> init_pool(X, SN) end, maps:get(pools, SubNet, [])),
    lists:foreach(fun(X) -> init_pd_pool(X, SN) end, maps:get(pd_pools, SubNet, [])),
    init_allocated(SubNet),
    SubNet.

init_pool(#{pool := {Min, Max}}, SN) ->
    init_available(ip2int(Min), ip2int(Max), 128, SN).

init_pd_pool(#{prefix := Prefix, prefix_len := PrefixLen, delegated_len := PdLen}, SN) ->
    PSize = (1 bsl PrefixLen) - 1,
    PMask = bnot PSize,
    Min = ip2int(Prefix) band PMask,
    Max = Min + PSize,
    init_available(Min, Max, PdLen, SN).

init_available(X, Max, _Len, _SN) when X > Max ->
    ok;
init_available(X, Max, Len, SN) ->
    ets:insert(?ADDRESS, #address{ip = {X, Len}, subnet = SN, status = available}),
    Next = X + 1 bsl (128 - Len),
    init_available(Next, Max, Len, SN).

init_allocated(#{options := Options}) ->
    DateTime = {date(), time()},
    Now = calendar:datetime_to_gregorian_seconds(DateTime),
    Allocate = fun(#lease{clientid = ClientId, ip = IP} = L, Acc)
		     when ?IS_NOT_EXPIRED(L, Now) ->
		       allocate_address(ClientId, IP, Options),
		       [ClientId | Acc];
		  (_, Acc) ->
		       Acc
	       end,
    dets:foldl(Allocate, [], ?LEASE).

get_cfg(Key, Config, Default) ->
    AppV = application:get_env(dhcpv6, Key, Default),
    proplists:get_value(Key, Config, AppV).

lease_time(Opts) ->
    Preferred = get_cfg(preferred_lifetime, Opts, 3600),
    Valid = get_cfg(valid_lifetime, Opts, 7200),
    {Preferred, Valid}.
