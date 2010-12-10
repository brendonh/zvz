%%%-------------------------------------------------------------------
%%% File    : zvz_app.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : ZVZ Server
%%%
%%% Created : 11 Dec 2010 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------

-module(zvz_app).

-behaviour(application).

%% API
-export([launch/0, test/1]).

%% Application callbacks
-export([start/2, stop/1]).


%%====================================================================
%% API
%%====================================================================

launch() ->
    application:start(zvz).

test(Modules) ->
    eunit:test({inorder, Modules}, [verbose]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_Type, _StartArgs) ->
    case zvz_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.


stop(_State) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
