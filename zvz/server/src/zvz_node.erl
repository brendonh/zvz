%%%-------------------------------------------------------------------
%%% File    : zvz_node.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : A single ZVZ game
%%%
%%% Created :  6 Dec 2010 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(zvz_node).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-include("zvz.hrl").
-include("zvz_util.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal callbacks
-export([broadcast/2, cast_player/3]).

-record(state, {
  uuid,
  players
}).

-define(MIMETYPE, "application/x-amf").

%%====================================================================
%% API
%%====================================================================

start_link(UUID) ->
    gen_server:start_link(?MODULE, [UUID], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([UUID]) ->
    {ok, #state{
       uuid = binary_to_list(UUID),
       players = gb_trees:empty()
      }}.


handle_call({register, PlayerID}, {Pid, _}, State) ->
    ?DBG({registering, PlayerID, Pid}),
    NewPlayers = case gb_trees:lookup(PlayerID, State#state.players) of
                     {value, OldPids} ->
                         %gen_server:cast(OldPid, ghosted),
                         gb_trees:update(PlayerID, [Pid|OldPids], State#state.players);
                     none ->
                         gb_trees:insert(PlayerID, [Pid], State#state.players)
                 end,
    NewState = State#state{players=NewPlayers},
    {reply, ok, NewState};

handle_call(Request, _From, State) ->
    ?DBG({unknown_call, Request}),
    Reply = ok,
    {reply, Reply, State}.



handle_cast(Msg, State) ->
    ?DBG({unknown_cast, Msg}),
    {noreply, State}.


handle_info(Info, State) ->
    ?DBG({unknown_info, Info}),
    {noreply, State}.


terminate(Reason, _State) ->
    ?DBG({terminating, Reason}),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%%% Dispatch
%%--------------------------------------------------------------------

broadcast(Message, State) ->
    [[gen_server:cast(Pid, Message) || Pid <- Pids]
     || Pids <- gb_trees:values(State#state.players)].


cast_player(PlayerID, Message, State) ->
    case gb_trees:lookup(PlayerID, State#state.players) of
        {value, Pids} ->
            [gen_server:cast(Pid, Message) || Pid <- Pids];
        _ -> ok
    end.



%%--------------------------------------------------------------------
%%% Upload handling
%%--------------------------------------------------------------------
