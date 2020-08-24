%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(dhcpv6_session).

-callback allocated(ClientId :: dhcpv6_server:client_id(),
		    ChAddr :: dhcpv6_server:chaddr_id(),
		    IP :: inet:ip6_address(),
		    Options :: [{integer(), any()}]) -> 'ok'.

-callback informed(ClientId :: dhcpv6_server:client_id(),
		   IP :: inet:ip6_address(),
		   Options :: [{integer(), any()}]) -> 'ok'.

-callback expired(ClientId :: dhcpv6_server:client_id(),
		  IP :: inet:ip6_address()) -> 'ok'.

-callback released(ClientId :: dhcpv6_server:client_id(),
		   IP :: inet:ip6_address()) -> 'ok'.
