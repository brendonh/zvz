%%%-------------------------------------------------------------------
%%% File    : zvz_node_test.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : ZVZ game tests
%%%
%%% Created : 11 Dec 2010 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(zvz_node_test).

-include_lib("eunit/include/eunit.hrl").

-include("zvz.hrl").

setup() ->
    application:load(zvz),
    {ok, Pid} = zvz_node:start_link(<<"test">>),
    Pid.

player_registration_test() ->
    Pid = setup(),
    Call = fun(R) -> gen_server:call(Pid, R) end,
    Players = fun() -> State = Call(get_state), State#gameState.players end, 

    % Initial empty state
    ?assertMatch({none, none}, Players()),
    
    Self = self(),

    % Register one player
    ?assertMatch(ok, Call({register, one})),
    ?assertMatch({#player{uuid=one, pid=Self, side=left}, none}, Players()),
    
    % Then the other
    ?assertMatch(ok, Call({register, two})),
    ?assertMatch({#player{uuid=one, pid=Self, side=left}, #
                  player{uuid=two, pid=Self, side=right}}, 
                 Players()),
    
    % But no more than two
    ?assertMatch({error, _}, Call({register, three})),
    ?assertMatch({#player{uuid=one, pid=Self, side=left}, 
                  #player{uuid=two, pid=Self, side=right}}, 
                 Players()),
    
    % Re-register an existing player
    NewPid = spawn(fun() -> Self ! {done, Call({register, one})} end),
    
    receive
        {done, Result} -> ok
    end,
    
    ?assertMatch(ok, Result),
    ?assertMatch({#player{uuid=one, pid=NewPid, side=left}, 
                  #player{uuid=two, pid=Self, side=right}}, 
                 Players()).


map_test() ->
    Map = zvz_node:generate_map(1, 1),
    ?assertMatch(#tile{row=0, col=0, left=[], right=[]},
                zvz_node:get_tile(Map, 0, 0)),
    
    Map2 = zvz_node:set_tile_left(Map, 0, 0, [hey]),
    ?assertMatch(#tile{row=0, col=0, left=[hey], right=[]},
                zvz_node:get_tile(Map2, 0, 0)),

    Map3 = zvz_node:set_tile_right(Map2, 0, 0, [there]),
    ?assertMatch(#tile{row=0, col=0, left=[hey], right=[there]},
                zvz_node:get_tile(Map3, 0, 0)).
    

unit_placing_test() ->
    Map = zvz_node:generate_map(?ROWS, ?COLUMNS),
    Player1 = #player{uuid= <<"one">>, gold=25, pid=self(), side=left},
    Player2 = #player{uuid= <<"one">>, gold=25, pid=self(), side=right},

    ?assertMatch({error, invalid_row}, zvz_node:place_unit(Map, Player1, grunt, -1)),
    ?assertMatch({error, invalid_row}, zvz_node:place_unit(Map, Player1, grunt, ?ROWS+1)),
    
    NewMap = lists:foldl(
               fun(I, OldMap) -> 
                       {ok, NewMap} = zvz_node:place_unit(OldMap, Player1, I, 3),
                       NewMap
               end,
               Map, lists:seq(1, 5)),
    
    ?assertMatch(#tile{row=3, col=0, left=[5,4,3,2,1], right=[]},
                 zvz_node:get_tile(NewMap, 3, 0)),
    
    ?assertMatch({error, tile_full},
                 zvz_node:place_unit(NewMap, Player1, grunt, 3)),

    NewMap2 = lists:foldl(
                fun(I, OldMap) -> 
                        {ok, NewMap2} = zvz_node:place_unit(OldMap, Player2, I, 3),
                        NewMap2
                end,
                NewMap, lists:seq(1, 5)),
    
    ?assertMatch(#tile{row=3, col=?COLUMNS-1, 
                       left=[], 
                       right=[5,4,3,2,1]},
                 zvz_node:get_tile(NewMap2, 3, ?COLUMNS-1)),
    
    ?assertMatch({error, tile_full},
                 zvz_node:place_unit(NewMap2, Player2, grunt, 3)).



mt(C, L, R) -> 
    #tile{row=0, col=C, 
          left=[#unit{name=LN, move=LM, owner=left} || {LN, LM} <- L],
          right=[#unit{name=RN, move=RM, owner=right} || {RN, RM} <- R]}.


test_turn_movement(From, To) ->
    test_turn_movement(From, To, false).

test_turn_movement(From, To, Print) ->

    NewRow = zvz_node:turn_movement(array:from_list(From), 0),

    Result = lists:sublist(array:to_list(NewRow), 1, length(To)),

    case Print of
      true -> ?debugVal(Result);
      _ -> ok
    end,

    ?assertMatch(Result, To).


turn_movement_empty_test() ->
    test_turn_movement([], []).


turn_movement_still_test() ->
    test_turn_movement(
      [mt(0, [{still, 0}], [])],
      [mt(0, [{still, 0}], [])]).


turn_movement_left_move_test() ->
      test_turn_movement(
      [mt(0, [{left, 1}], [])],
      [mt(0, [], []),
       mt(1, [{left, 1}], [])]).


turn_movement_right_move_test() ->
      test_turn_movement(
       [mt(0, [], []), mt(1, [], [{right, 1}])],
       [mt(0, [], [{right, 1}]), mt(1, [], [])]).


turn_movement_right_end_test() ->
      test_turn_movement(
       [mt(0, [], []), mt(1, [], [{right, 5}])],
       [mt(0, [], [{right, 5}]), mt(1, [], [])]).

turn_movement_left_end_test() ->
      test_turn_movement(
       [mt(X, [], []) || X <- lists:seq(0, 12)] ++ [mt(13, [{left, 5}], [])],
       [mt(X, [], []) || X <- lists:seq(0, 13)] ++ [mt(14, [{left, 5}], [])]).

turn_movement_multi_test() ->

    From = [mt(0, [], []),
            mt(1, [{one, 2}, {three, 0}], []), 
            mt(2, [], []), 
            mt(3, [], []), 
            mt(4, [], []), 
            mt(5, [], [{two, 1}, {four, 3}])],
 
    To = [mt(0, [], []),
          mt(1, [{three, 0}], []),
          mt(2, [], []),
          mt(3, [{one, 2}], [{four, 3}]),
          mt(4, [], [{two, 1}]),
          mt(5, [], [])],

    test_turn_movement(From, To).


    
