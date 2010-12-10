%%%-------------------------------------------------------------------
%%% File    : zvz_node_sup.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : Supervise zvz nodes
%%%
%%% Created : 11 Dec 2010 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(zvz_node_sup).

-behaviour(supervisor).

-include("zvz_util.hrl").

%% API
-export([start_link/0, get_game/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


get_game(UUID) ->
    case ets:lookup(zvzs, UUID) of
        [{UUID, Pid}] -> {ok, Pid};
        [] ->
            {ok, Pid} = supervisor:start_child(?MODULE, [UUID]),
            case ets:insert_new(zvzs, {UUID, Pid}) of
                true ->
                    gen_server:cast(Pid, load),
                    {ok, Pid};
                false ->
                    gen_server:cast(Pid, die),
                    get_game(UUID)
            end
    end.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->

    ets:new(zvzs, [public, set, named_table]),

    Spec = {conn,{zvz_node,start_link,[]},
            transient,5000,worker,[zvz_node]},
    {ok,{{simple_one_for_one,0,1}, [Spec]}}.


%%====================================================================
%% Internal functions
%%====================================================================
