%%%-------------------------------------------------------------------
%%% File    : zvz_manager.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Manage ZVZ games
%%%
%%% Created : 11 Dec 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(zvz_manager).

-behaviour(gen_server).

-include("zvz.hrl").
-include("zvz_util.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
}).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?DBG({?MODULE, starting_up}),
    process_flag(trap_exit, true),

    {ok, #state{}}.


handle_call({login, Email, Zvz}, {Pid, _}, State) ->
    Reply = handle_login(Email, Zvz, Pid, State),
    {reply, Reply, State};


handle_call(Request, _From, State) ->
    ?DBG({unexpected_call, Request}),
    Reply = ok,
    {reply, Reply, State}.



handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.


handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ?DBG(shutdown),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%====================================================================
%% Outgoing
%%====================================================================


%%====================================================================
%% Login / Connection API handlers
%%====================================================================

handle_login(Email, _Zvz, _Pid, _State) ->
    {ok, {Email, Email}}.





