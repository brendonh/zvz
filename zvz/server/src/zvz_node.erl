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

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal callbacks
-export([broadcast/2, cast_player/3]).

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
    {ok, Units} = application:get_env(zvz, units),
    {ok, #gameState{
       uuid = binary_to_list(UUID),
       players = {none, none},
       units = Units,
       map = generate_map(?ROWS, ?COLUMNS)
      }}.


handle_call({register, PlayerID}, {Pid, _}, GameState) ->
    {ok, Gold} = application:get_env(zvz, initial_gold),
    NewPlayer = #player{uuid=PlayerID, pid=Pid, gold=Gold},

    {Players, Reply} = case GameState#gameState.players of
                           {Other, #player{uuid=PlayerID}=Old} ->
                               {{Other, Old#player{pid=Pid}}, ok};
                           {#player{uuid=PlayerID}=Old, Other} ->
                               {{Old#player{pid=Pid}, Other}, ok};
                           {none, Other} ->
                               {{NewPlayer#player{side=left}, Other}, ok};
                           {Other, none} ->
                               {{Other, NewPlayer#player{side=right}}, ok};
                           Full ->
                               {Full, {error, no_free_side}}
                       end,
    {reply, Reply, GameState#gameState{players=Players}};


%% For unit testing and debugging
handle_call(get_state, _From, GameState) ->
    {reply, GameState, GameState};

handle_call(Request, _From, GameState) ->
    ?DBG({unknown_call, Request}),
    Reply = ok,
    {reply, Reply, GameState}.



handle_cast(Msg, GameState) ->
    ?DBG({unknown_cast, Msg}),
    {noreply, GameState}.


handle_info(Info, GameState) ->
    ?DBG({unknown_info, Info}),
    {noreply, GameState}.


terminate(Reason, _GameState) ->
    ?DBG({terminating, Reason}),
    ok.


code_change(_OldVsn, GameState, _Extra) ->
    {ok, GameState}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

generate_map(Rows, Cols) ->
    array:fix(array:from_list(
      [array:fix(array:from_list(
        [#tile{row=Row, col=Col}
         || Col <- lists:seq(0, Cols-1)]))
       || Row <- lists:seq(0, Rows-1)]
    )).

get_tile(Map, Row, Col) ->
    array:get(Col, array:get(Row, Map)).

set_tile_left(Map, Row, Col, Units) -> set_tile(Map, Row, Col, #tile.left, Units).
set_tile_right(Map, Row, Col, Units) -> set_tile(Map, Row, Col, #tile.right, Units).
    
set_tile(Map, Row, Col, Element, Units) ->
    OldRow = array:get(Row, Map),
    OldTile = array:get(Col, OldRow),
    NewTile = setelement(Element, OldTile, Units),
    NewRow = array:set(Col, NewTile, OldRow),
    array:set(Row, NewRow, Map).

%%--------------------------------------------------------------------
%%% Unit placing
%%--------------------------------------------------------------------

place_unit(_Map, _Player, _Unit, Row) when Row < 0 orelse Row >= ?ROWS ->
    {error, invalid_row};

place_unit(_Map, #player{gold=Gold},  #unitType{cost=Cost}, _Row) when Gold < Cost ->
    {error, insufficient_gold};

place_unit(Map, Player, Unit, Row) ->
    {Col, UnitsPos} = case Player#player.side of
                          left -> {0, #tile.left};
                          right -> {?COLUMNS-1, #tile.right}
                      end,

    Tile = get_tile(Map, Row, Col),
    Units = element(UnitsPos, Tile),

    case length(Units) of
        X when X >= ?UNITS_PER_TILE ->
            {error, tile_full};
        _ -> 
            {ok, set_tile(Map, Row, Col, UnitsPos, [Unit|Units])}            
    end.


%%--------------------------------------------------------------------
%%% Dispatch
%%--------------------------------------------------------------------

broadcast(Message, GameState) ->
    [[gen_server:cast(Pid, Message) || Pid <- Pids]
     || Pids <- gb_trees:values(GameState#gameState.players)].


cast_player(PlayerID, Message, GameState) ->
    case gb_trees:lookup(PlayerID, GameState#gameState.players) of
        {value, Pids} ->
            [gen_server:cast(Pid, Message) || Pid <- Pids];
        _ -> ok
    end.



%%--------------------------------------------------------------------
%%% Upload handling
%%--------------------------------------------------------------------
