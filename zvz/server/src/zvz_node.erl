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
%%% Map manipulation
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
%%% Turns -- Movement
%%--------------------------------------------------------------------
    
turn_movement(Row, RowNum) ->
    RowList = array:to_list(Row),
    {LeftUnits, RightUnits} = get_units(RowList, [], []),

    Boundary = case find_existing_boundary(RowList) of
                   none ->
                       find_new_boundary(LeftUnits, RightUnits);
                   Col -> Col
               end,
    
    NewLeft = [move_unit(C, U, Boundary, 1) || {C, U} <- LeftUnits],
    NewRight = [move_unit(C, U, Boundary, -1) || {C, U} <- RightUnits],
    
    Units = lists:sort(NewLeft ++ NewRight),

    array:from_list(rebuild_row(Units, RowNum)).


get_units([], Left, Right) ->
    {lists:flatten(Left), lists:flatten(Right)};
get_units([#tile{left=L, right=R, col=C}|Rest], Left, Right) ->
    get_units(Rest, 
              [[{C,U} || U <- L]|Left], 
              [[{C,U} || U <- R]|Right]).


find_existing_boundary([]) ->
    none;
find_existing_boundary([#tile{left=[]}|Rest]) ->
    find_existing_boundary(Rest);
find_existing_boundary([#tile{right=[]}|Rest]) ->
    find_existing_boundary(Rest);
find_existing_boundary([#tile{col=Col}|_]) ->
    Col.


find_new_boundary(Left, Right) ->
    Collisions = lists:flatten(
                   [collide(L, R) || L <- Left,
                                     R <- Right]),

    case lists:sort([C || C <- Collisions, C /= none]) of
        [] -> none;
        [{Delta, Col}|_Rest] -> Col
    end.


collide({LeftCol, #unit{move=LeftMove}}, {RightCol, #unit{move=RightMove}}) 
  when LeftMove /= 0 orelse RightMove /= 0 ->
    case (RightCol - LeftCol) / (LeftMove + RightMove) of
        D when D > 1 -> none;
        Delta -> 
            {Delta, round_collide(LeftCol + (Delta * LeftMove))}
    end;
collide(_, _) -> none.


round_collide(Col) when (Col - trunc(Col)) == 0.5 -> 
    FlipFlop = random:uniform(2) - 1,
    trunc(Col) + FlipFlop;
round_collide(Other) -> round(Other).


move_unit(Col, #unit{move=Move}=Unit, Boundary, Direction) ->
    NewCol = erlang:min(?COLUMNS - 1, erlang:max(0, Col + (Move * Direction))),
    case Boundary of
        none -> {NewCol, Unit};
        _ -> 
            case (NewCol - Boundary) * Direction of
                X when X > 0 -> {Boundary, Unit};
                _ -> {NewCol, Unit}
            end
    end.


rebuild_row(Units, RowNum) ->
    rebuild_row(Units, [#tile{row=RowNum, col=0}], RowNum, 0).


rebuild_row(Units, Row, RowNum, Col) when Col == ?COLUMNS ->
    lists:reverse(Row);

rebuild_row([{Col, #unit{owner=Owner}=Unit}|Rest], 
            [#tile{col=Col}=Tile|Row], 
            RowNum, Col) ->
    UnitPos = case Owner of left -> #tile.left; right -> #tile.right end,
    Units = element(UnitPos, Tile),
    rebuild_row(Rest, [setelement(UnitPos, Tile, [Unit|Units])|Row], RowNum, Col);

rebuild_row(Units, Row, RowNum, Col) ->
    %?debugVal({Col, Row, hd(Units)}),
    Tile = #tile{row=RowNum, col=Col+1},
    rebuild_row(Units, [Tile|Row], RowNum, Col+1).
    

                     

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
