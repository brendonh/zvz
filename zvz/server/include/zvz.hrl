%%%-------------------------------------------------------------------
%%% File    : zvz.hrl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Constants and records
%%%
%%% Created : 11 Dec 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------

-define(ROWS, 5).
-define(COLUMNS, 15).
-define(UNITS_PER_TILE, 5).

-record(gameState, {
  uuid,
  players,
  units,
  map
}).

-record(player, {
  uuid,
  gold,
  pid,
  side
}).

-record(unitType, {
  name,
  cost,
  move,
  attack,
  health
}).

-record(tile, {
  row,
  col,
  left=[],
  right=[]
}).

-record(unit, {
  name,
  owner,
  move,
  attack,
  health
}).
