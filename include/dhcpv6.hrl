%% Copyright 2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-define(DHCPv6_SERVER_PORT, 547).
-define(DHCPv6_CLIENT_PORT, 546).
-define(DHCPv6_NODE_LOCAL_ALL_ROUTERS, {16#ff01, 0, 0, 0, 0, 0, 0, 2}).
-define(ALL_DHCPv6_RELAY_AGENTS_AND_SERVERS, {16#ff02, 0, 0, 0, 0, 0, 1, 2}).
-define(ALL_DHCPv6_SERVERS, {16#ff05, 0, 0, 0, 0, 0, 1, 3}).

-record(dhcpv6, {
	  op,                       %% Message opcode
	  xid,                      %% Transaction ID
	  options = #{}             %% Optional parameters
	 }).

-record(dhcpv6_relay, {
	  op,                       %% Message opcode
	  hops,                     %% Number of relay agents that have relayed this message.
	  link_addr,                %% A global or site-local address that will be used by
				    %% the server to identify the link on which the client
				    %% is located.
	  peer_addr,                %% The address of the client or relay agent from which
				    %% the message to be relayed was received.
	  options = #{}             %% Optional parameters
	 }).

%%% DHCPv6 Option codes
-define(D6O_CLIENTID,                             1).                  %% RFC 8415
-define(D6O_SERVERID,                             2).
-define(D6O_IA_NA,                                3).
-define(D6O_IA_TA,                                4).
-define(D6O_IAADDR,                               5).
-define(D6O_ORO,                                  6).
-define(D6O_PREFERENCE,                           7).
-define(D6O_ELAPSED_TIME,                         8).
-define(D6O_RELAY_MSG,                            9).
%% Option code 10 unassigned.
-define(D6O_AUTH,                                 11).
-define(D6O_UNICAST,                              12).
-define(D6O_STATUS_CODE,                          13).
-define(D6O_RAPID_COMMIT,                         14).
-define(D6O_USER_CLASS,                           15).
-define(D6O_VENDOR_CLASS,                         16).
-define(D6O_VENDOR_OPTS,                          17).
-define(D6O_INTERFACE_ID,                         18).
-define(D6O_RECONF_MSG,                           19).
-define(D6O_RECONF_ACCEPT,                        20).
-define(D6O_SIP_SERVERS_DNS,                      21).                  %% RFC3319
-define(D6O_SIP_SERVERS_ADDR,                     22).                  %% RFC3319
-define(D6O_NAME_SERVERS,                         23).                  %% RFC3646
-define(D6O_DOMAIN_SEARCH,                        24).                  %% RFC3646
-define(D6O_IA_PD,                                25).                  %% RFC8415
-define(D6O_IAPREFIX,                             26).                  %% RFC8415
-define(D6O_NIS_SERVERS,                          27).                  %% RFC3898
-define(D6O_NISP_SERVERS,                         28).                  %% RFC3898
-define(D6O_NIS_DOMAIN_NAME,                      29).                  %% RFC3898
-define(D6O_NISP_DOMAIN_NAME,                     30).                  %% RFC3898
-define(D6O_SNTP_SERVERS,                         31).                  %% RFC4075
-define(D6O_INFORMATION_REFRESH_TIME,             32).                  %% RFC8415
-define(D6O_BCMCS_SERVER_D,                       33).                  %% RFC4280
-define(D6O_BCMCS_SERVER_A,                       34).                  %% RFC4280
%% 35 is unassigned
-define(D6O_GEOCONF_CIVIC,                        36).                  %% RFC4776
-define(D6O_REMOTE_ID,                            37).                  %% RFC4649
-define(D6O_SUBSCRIBER_ID,                        38).                  %% RFC4580
-define(D6O_CLIENT_FQDN,                          39).                  %% RFC4704
-define(D6O_PANA_AGENT,                           40).                  %% paa-option
-define(D6O_NEW_POSIX_TIMEZONE,                   41).                  %% RFC4833
-define(D6O_NEW_TZDB_TIMEZONE,                    42).                  %% RFC4833
-define(D6O_ERO,                                  43).                  %% RFC4994
-define(D6O_LQ_QUERY,                             44).                  %% RFC5007
-define(D6O_CLIENT_DATA,                          45).                  %% RFC5007
-define(D6O_CLT_TIME,                             46).                  %% RFC5007
-define(D6O_LQ_RELAY_DATA,                        47).                  %% RFC5007
-define(D6O_LQ_CLIENT_LINK,                       48).                  %% RFC5007
-define(D6O_MIP6_HNIDF,                           49).                  %% RFC6610
-define(D6O_MIP6_VDINF,                           50).                  %% RFC6610
-define(D6O_V6_LOST,                              51).                  %% RFC5223
-define(D6O_CAPWAP_AC_V6,                         52).                  %% RFC5417
-define(D6O_RELAY_ID,                             53).                  %% RFC5460
-define(D6O_IPV6_ADDRESS_MOS,                     54).                  %% RFC5678
-define(D6O_IPV6_FQDN_MOS,                        55).                  %% RFC5678
-define(D6O_NTP_SERVER,                           56).                  %% RFC5908
-define(D6O_V6_ACCESS_DOMAIN,                     57).                  %% RFC5986
-define(D6O_SIP_UA_CS_LIST,                       58).                  %% RFC6011
-define(D6O_BOOTFILE_URL,                         59).                  %% RFC5970
-define(D6O_BOOTFILE_PARAM,                       60).                  %% RFC5970
-define(D6O_CLIENT_ARCH_TYPE,                     61).                  %% RFC5970
-define(D6O_NII,                                  62).                  %% RFC5970
-define(D6O_GEOLOCATION,                          63).                  %% RFC6225
-define(D6O_AFTR_NAME,                            64).                  %% RFC6334
-define(D6O_ERP_LOCAL_DOMAIN_NAME,                65).                  %% RFC6440
-define(D6O_RSOO,                                 66).                  %% RFC6422
-define(D6O_PD_EXCLUDE,                           67).                  %% RFC6603
-define(D6O_VSS,                                  68).                  %% RFC6607
-define(D6O_MIP6_IDINF,                           69).                  %% RFC6610
-define(D6O_MIP6_UDINF,                           70).                  %% RFC6610
-define(D6O_MIP6_HNP,                             71).                  %% RFC6610
-define(D6O_MIP6_HAA,                             72).                  %% RFC6610
-define(D6O_MIP6_HAF,                             73).                  %% RFC6610
-define(D6O_RDNSS_SELECTION,                      74).                  %% RFC6731
-define(D6O_KRB_PRINCIPAL_NAME,                   75).                  %% RFC6784
-define(D6O_KRB_REALM_NAME,                       76).                  %% RFC6784
-define(D6O_KRB_DEFAULT_REALM_NAME,               77).                  %% RFC6784
-define(D6O_KRB_KDC,                              78).                  %% RFC6784
-define(D6O_CLIENT_LINKLAYER_ADDR,                79).                  %% RFC6939
-define(D6O_LINK_ADDRESS,                         80).                  %% RFC6977
-define(D6O_RADIUS,                               81).                  %% RFC7037
-define(D6O_SOL_MAX_RT,                           82).                  %% RFC8415
-define(D6O_INF_MAX_RT,                           83).                  %% RFC8415
-define(D6O_ADDRSEL,                              84).                  %% RFC7078
-define(D6O_ADDRSEL_TABLE,                        85).                  %% RFC7078
-define(D6O_V6_PCP_SERVER,                        86).                  %% RFC7291
-define(D6O_DHCPV4_MSG,                           87).                  %% RFC7341
-define(D6O_DHCPV4_O_DHCPV6_SERVER,               88).                  %% RFC7341
-define(D6O_S46_RULE,                             89).                  %% RFC7598
-define(D6O_S46_BR,                               90).                  %% RFC7598
-define(D6O_S46_DMR,                              91).                  %% RFC7598
-define(D6O_S46_V4V6BIND,                         92).                  %% RFC7598
-define(D6O_S46_PORTPARAMS,                       93).                  %% RFC7598
-define(D6O_S46_CONT_MAPE,                        94).                  %% RFC7598
-define(D6O_S46_CONT_MAPT,                        95).                  %% RFC7598
-define(D6O_S46_CONT_LW,                          96).                  %% RFC7598
-define(D6O_4RD,                                  97).                  %% RFC7600
-define(D6O_4RD_MAP_RULE,                         98).                  %% RFC7600
-define(D6O_4RD_NON_MAP_RULE,                     99).                  %% RFC7600
-define(D6O_LQ_BASE_TIME,                        100).                  %% RFC7653
-define(D6O_LQ_START_TIME,                       101).                  %% RFC7653
-define(D6O_LQ_END_TIME,                         102).                  %% RFC7653
-define(D6O_V6_CAPTIVE_PORTAL,                   103).                  %% RFC7710
-define(D6O_MPL_PARAMETERS,                      104).                  %% RFC7774
-define(D6O_ANI_ATT,                             105).                  %% RFC7839
-define(D6O_ANI_NETWORK_NAME,                    106).                  %% RFC7839
-define(D6O_ANI_AP_NAME,                         107).                  %% RFC7839
-define(D6O_ANI_AP_BSSID,                        108).                  %% RFC7839
-define(D6O_ANI_OPERATOR_ID,                     109).                  %% RFC7839
-define(D6O_ANI_OPERATOR_REALM,                  110).                  %% RFC7839
-define(D6O_S46_PRIORITY,                        111).                  %% RFC8026
%% 112 unassigned
-define(D6O_V6_PREFIX64,                         113).                  %% RFC8115
-define(D6O_F_BINDING_STATUS,                    114).                  %% RFC8156
-define(D6O_F_CONNECT_FLAGS,                     115).                  %% RFC8156
-define(D6O_F_DNS_REMOVAL_INFO,                  116).                  %% RFC8156
-define(D6O_F_DNS_HOST_NAME,                     117).                  %% RFC8156
-define(D6O_F_DNS_ZONE_NAME,                     118).                  %% RFC8156
-define(D6O_F_DNS_FLAGS,                         119).                  %% RFC8156
-define(D6O_F_EXPIRATION_TIME,                   120).                  %% RFC8156
-define(D6O_F_MAX_UNACKED_BNDUPD,                121).                  %% RFC8156
-define(D6O_F_MCLT,                              122).                  %% RFC8156
-define(D6O_F_PARTNER_LIFETIME,                  123).                  %% RFC8156
-define(D6O_F_PARTNER_LIFETIME_SENT,             124).                  %% RFC8156
-define(D6O_F_PARTNER_DOWN_TIME,                 125).                  %% RFC8156
-define(D6O_F_PARTNER_RAW_CLT_TIME,              126).                  %% RFC8156
-define(D6O_F_PROTOCOL_VERSION,                  127).                  %% RFC8156
-define(D6O_F_KEEPALIVE_TIME,                    128).                  %% RFC8156
-define(D6O_F_RECONFIGURE_DATA,                  129).                  %% RFC8156
-define(D6O_F_RELATIONSHIP_NAME,                 130).                  %% RFC8156
-define(D6O_F_SERVER_FLAGS,                      131).                  %% RFC8156
-define(D6O_F_SERVER_STATE,                      132).                  %% RFC8156
-define(D6O_F_START_TIME_OF_STATE,               133).                  %% RFC8156
-define(D6O_F_STATE_EXPIRATION_TIME,             134).                  %% RFC8156
-define(D6O_RELAY_SOURCE_PORT,                   135).                  %% RFC8357
%% 136-142 unassigned
-define(D6O_IPV6_ADDRESS_ANDSF,                  143).                  %% RFC6153


%%% DHCPv6 Message types
-define(DHCPV6_SOLICIT,               1).
-define(DHCPV6_ADVERTISE,             2).
-define(DHCPV6_REQUEST,               3).
-define(DHCPV6_CONFIRM,               4).
-define(DHCPV6_RENEW,                 5).
-define(DHCPV6_REBIND,                6).
-define(DHCPV6_REPLY,                 7).
-define(DHCPV6_RELEASE,               8).
-define(DHCPV6_DECLINE,               9).
-define(DHCPV6_RECONFIGURE,           10).
-define(DHCPV6_INFORMATION_REQUEST,   11).
-define(DHCPV6_RELAY_FORW,            12).
-define(DHCPV6_RELAY_REPL,            13).
%% RFC 5007
-define(DHCPV6_LEASEQUERY,            14).
-define(DHCPV6_LEASEQUERY_REPLY,      15).
%% RFC 5460
-define(DHCPV6_LEASEQUERY_DONE,       16).
-define(DHCPV6_LEASEQUERY_DATA,       17).
%% RFC 6977
-define(DHCPV6_RECONFIGURE_REQUEST,   18).
-define(DHCPV6_RECONFIGURE_REPLY,     19).
%% RFC 7341
-define(DHCPV6_DHCPV4_QUERY,          20).
-define(DHCPV6_DHCPV4_RESPONSE,       21
%% RFC 7653
DHCPV6_ACTIVELEASEQUERY     = 22).
-define(DHCPV6_STARTTLS,              23).
%% RFC 8156
-define(DHCPV6_BNDUPD,                24).
-define(DHCPV6_BNDREPLY,              25).
-define(DHCPV6_POOLREQ,               26).
-define(DHCPV6_POOLRESP,              27).
-define(DHCPV6_UPDREQ,                28).
-define(DHCPV6_UPDREQALL,             29).
-define(DHCPV6_UPDDONE,               30).
-define(DHCPV6_CONNECT,               31).
-define(DHCPV6_CONNECTREPLY,          32).
-define(DHCPV6_DISCONNECT,            33).
-define(DHCPV6_STATE,                 34).
-define(DHCPV6_CONTACT,               35).
