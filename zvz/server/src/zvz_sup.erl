%%%-------------------------------------------------------------------
%%% File    : zvz_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Top-level zvz supervisor
%%%
%%% Created : 11 Dec 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(zvz_sup).

-behaviour(supervisor).

-include("zvz_util.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->

    {ok, TcpPort} = application:get_env(tcp_port),
    {ok, HttpPort} = application:get_env(http_port),

    Manager = {manager, {zvz_manager, start_link, []},
               permanent,2000,worker,[zvz_manager]},
    
    ConnSup = {connsup, {zvz_conn_sup, start_link, []},
               permanent,2000,supervisor,[zvz_conn_sup]},

    ZvzSup = {zvzsup, {zvz_node_sup, start_link, []},
                permanent,2000,supervisor,[zvz_node_sup]},

    Listener = {listener, {zvz_tcp_listener, start_link, [TcpPort]},
              permanent,2000,worker,[zvz_tcp_listener]},

    Mochiweb = {mochiweb, {mochiweb_http, start,
                           [[{name, mochi_zvz},
                             {port, HttpPort}, 
                             {loop, fun zvz_http:respond/1}]]},
                permanent,2000,worker,[mochiweb_socket_server]},

    {ok,{{one_for_one,0,1}, [Manager, ConnSup, ZvzSup, Listener, Mochiweb]}}.


%%====================================================================
%% Internal functions
%%====================================================================
