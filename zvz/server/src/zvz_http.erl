%%%-------------------------------------------------------------------
%%% File    : zvz_http.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : HTTP server for applet etc
%%%
%%% Created :  11 Dec 2010 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(zvz_http).

-export([respond/1]).

-define(STATIC_PATH, "priv/static").

respond(Req) -> dispatch(Req:get(method), Req:get(path), Req).


dispatch('GET', "/", Req) ->
    Req:serve_file("index.html", ?STATIC_PATH);

dispatch('GET', "/static/" ++ Path, Req) ->
    Req:serve_file(Path, ?STATIC_PATH);

dispatch(_, _, Req) ->
    Req:not_found().
