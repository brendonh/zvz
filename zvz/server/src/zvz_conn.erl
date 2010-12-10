%%%-------------------------------------------------------------------
%%% File    : zvz_conn.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 29 Apr 2010 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(zvz_conn).

-behaviour(gen_server).

-include("zvz.hrl").
-include("zvz_util.hrl").
-include("amf.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  socket,
  zvz,
  playerID
}).


-define(OBJ(PL), {object, <<"">>, PL}).
-define(PL(Obj), element(3, Obj)).

%%====================================================================
%% API
%%====================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Socket]) ->
    process_flag(trap_exit, true),
    {ok, {IP, _Port}} = inet:peername(Socket),
    ?DBG({client_conn_starting, Socket, IP}),
    {ok, #state{socket=Socket,
                playerID=none,
                zvz=none}}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast(socket_ready, #state{socket=Socket}=State) ->
    inet:setopts(Socket, [{active, once}]),  
    {noreply, State};

handle_cast(ghosted, State) ->
    ?DBG(ghosted),
    send_message(<<"ghosted">>, ?OBJ([]), State),
    gen_tcp:close(State#state.socket),
    {stop, normal, State};

handle_cast(Other, State) ->
    ?DBG({unexpected_cast, Other}),
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info({tcp, Socket, Bin}, #state{socket=Socket}=State) ->
    inet:setopts(Socket, [{active, once}]),
    Request = ?PL(element(1, amf3:decode(Bin))),
    Action = ?GV(<<"action">>, Request),
    Payload = ?PL(?GV(<<"payload">>, Request)),
    NewState = dispatch(Action, Payload, Request, State),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    ?DBG({client_disconnected, Socket}),
    {stop, normal, State};

handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    gen_server:cast(zvz_manager, {connection_closed, {State#state.playerID, self()}}),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Dispatch
%%--------------------------------------------------------------------

dispatch(Action, Payload, Request, #state{playerID=none}=State) ->
    Method = ?GV(<<"method">>, Request),
    case {Action, Method} of
        {<<"rpc">>, <<"login">>} ->
            handle_login(Payload, Request, State);
        _ ->
            Socket = State#state.socket,
            gen_tcp:send(Socket, <<"Log in first">>),
            gen_tcp:close(Socket),
            State
    end;

dispatch(<<"rpc">>, Payload, Request, State) ->
    ID = ?GV(<<"id">>, Request),
    Method = ?GV(<<"method">>, Request),
    handle_rpc(Method, Payload, ID, State);

dispatch(Action, _Payload, _Request, State) ->
    ?DBG({unknown_action, Action}),
    State.


handle_login(Creds, Req, State) ->
    ID = ?GV(<<"id">>, Req),
    Email = binary_to_list(?GV(<<"email">>, Creds)),
    Game = binary_to_list(?GV(<<"game">>, Creds)),
    ?DBG({login_attempt, Email, Game}),
    case gen_server:call(zvz_manager, {login, Email, Game}) of
        {error, _} -> 
            send_rpc_error(ID, <<"Invalid Credentials">>, State),
            State;
        {ok, {PlayerID, Name}} -> 
            ?DBG({login, Game, Email, PlayerID, Name}),
            case zvz_db:get_zvz(PlayerID, Game) of
                {ok, UUID} ->

                    NewState = State#state{zvz=UUID},

                    ok = zvz_call({register, PlayerID}, NewState),

                    send_rpc_reply(ID, ?OBJ([{id, list_to_binary(PlayerID)}, 
                                             {name, list_to_binary(Name)},
                                             {zvz, UUID}
                                             ]), NewState),

                    NewState#state{zvz=UUID, playerID=PlayerID};
                Other ->
                    ?DBG({oh_noes, Other}),
                    send_rpc_error(ID, <<"Game not found">>, State)
            end
    end.


handle_rpc(Method, Payload, _ID, State) ->
    ?DBG({unknown_rpc, Method, Payload}),
    State.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

zvz_call(Call, State) -> zvz_call(Call, State, 5000).

zvz_call(Call, State, Timeout) ->
    {ok, Pid} = zvz_node_sup:get_zvz(State#state.zvz),
    Result = gen_server:call(Pid, Call, Timeout),
    Result.


%%--------------------------------------------------------------------
%%% Sending
%%--------------------------------------------------------------------

send_rpc_reply(ID, Payload, State) ->
    send_message(<<"rpc_reply">>, Payload, [{id, ID}], State).


send_rpc_error(ID, ErrorMsg, State) when is_atom(ErrorMsg) ->
    BinError = list_to_binary(atom_to_list(ErrorMsg)),
    send_rpc_error(ID, BinError, State);
send_rpc_error(ID, ErrorMsg, State) ->
    send_message(<<"rpc_error">>, [], [{id, ID}, {error, ErrorMsg}], State).

send_message(Action, Payload, State) -> 
    send_message(Action, Payload, [], State).
    
send_message(Action, Payload, Extra, #state{socket=Socket}) ->
    Message = {object, <<"">>, 
               [{action, Action},
                {payload, Payload}
                |Extra]},
    gen_tcp:send(Socket, amf3:encode(Message)).

