%%%-------------------------------------------------------------------
%%% File    : zvz_conn_sup.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : Supervise client connections
%%%
%%% Created : 11 Dec 2010 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(zvz_conn_sup).

-behaviour(supervisor).

-include("zvz_util.hrl").

%% API
-export([start_link/0, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Spec = {conn,{zvz_conn,start_link,[]},
            transient,2000,worker,[zvz_conn]},
    {ok,{{simple_one_for_one,0,1}, [Spec]}}.


%%====================================================================
%% Internal functions
%%====================================================================
