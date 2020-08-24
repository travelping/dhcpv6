%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-define(ZERO_IPv6, {0,0,0,0,0,0,0,0}).

-record(host, {id, ip}).
-record(address, {ip, subnet, status, timer = undefined, options = undefined}).
-record(lease, {clientid, ip, expires}).
