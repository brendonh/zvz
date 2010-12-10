%%%-------------------------------------------------------------------
%%% File    : zvz_tcp_listener.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : TCP connection listener
%%%
%%% Created : 11 Dec 2010 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(zvz_tcp_listener).

-behaviour(gen_server).

-include("zvz_util.hrl").

%% External API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
  listener,
  acceptor
}).


%%====================================================================
%% API functions
%%====================================================================

start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {reuseaddr, true}, {packet, 4}, 
            {keepalive, true}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
        {ok, Socket} ->
            {ok, Ref} = prim_inet:async_accept(Socket, -1),
            ?DBG({listener_running, Ref}),
            {ok, #state{listener = Socket,
                        acceptor = Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.


handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
            ok -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,

        {ok, Pid} = zvz_conn_sup:start_client(CliSocket),
        gen_tcp:controlling_process(CliSocket, Pid),
        gen_server:cast(Pid, socket_ready),

        case prim_inet:async_accept(ListSock, -1) of
            {ok, NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
            error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
            {stop, Why, State}
    end;


handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok -> ok;
                Error -> gen_tcp:close(CliSocket), Error
            end;
        Error ->
            gen_tcp:close(CliSocket), Error
    end.
