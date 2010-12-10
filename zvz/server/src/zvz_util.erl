%%%-------------------------------------------------------------------
%%% File    : zvz_util.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Bits and bobs
%%%
%%% Created : 27 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(zvz_util).

-include("zvz.hrl").
-include("zvz_util.hrl").

-compile([export_all]).


%%====================================================================
%% Setup
%%====================================================================


%%====================================================================
%% UUIDs
%%====================================================================

parse_uuid(S) ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) -> 
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", 
                                [TL, TM, THV, CSR, CSL, N])).
    
random_uuid() -> 
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    <<(random:uniform(4294967296) - 1):32,
      (random:uniform(4294967296) - 1):32,
      (random:uniform(4294967296) - 1):32,
      (random:uniform(4294967296) - 1):32>>.


%%====================================================================
%% Time
%%====================================================================

unix_now() ->
    GregNow = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    GregEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0,0,0}}),
    GregNow - GregEpoch.
