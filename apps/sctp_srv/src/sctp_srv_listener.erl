%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2016 20:38
%%%-------------------------------------------------------------------
-module(sctp_srv_listener).
-author("regikul").

-behaviour(gen_server).

%% API
-export([
  start_link/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-include_lib("kernel/include/inet_sctp.hrl").

-record(state, {
  listen_on = 0 :: non_neg_integer(),
  socket :: gen_sctp:sctp_socket()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(pos_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port) when is_integer(Port), Port >= 1000, Port < 65000 ->
%%  Name = list_to_atom(integer_to_list(Port)),
  gen_server:start_link(?MODULE, [Port], []);
start_link(_Else) ->
  erlang:error(badarg, [_Else]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Port]) ->
  lager:info("trying to listen on ~p", [Port]),
  {ok, Socket} = gen_sctp:open(Port),
  ok = gen_sctp:listen(Socket, true),
  gen_server:cast(self(), recv),
  {ok, #state{listen_on = Port, socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(recv, #state{socket = Socket} = State) ->
  case gen_sctp:recv(Socket, 500) of
    {ok, {_FromIP, _FromPort, AncData, Data}} ->
      handle_sctp_input(Socket, AncData, Data);
    {error, timeout} -> ok;
    {error, _Reason} ->
      lager:error("can not read due to: ~p", [_Reason])
  end,
  gen_server:cast(self(), recv),
  {noreply, State};
handle_cast(_Request, State) ->
  lager:info("unknown req: ~p", [_Request]),
  {noreply, State}.

-type sctp_recv_data() :: string() |
                          binary() |
                          #sctp_sndrcvinfo{} |
                          #sctp_assoc_change{} |
                          #sctp_paddr_change{} |
                          #sctp_adaptation_event{} |
                          #sctp_shutdown_event{}.
-spec handle_sctp_input(gen_sctp:sctp_socket(), [#sctp_sndrcvinfo{}], sctp_recv_data()) -> ok.
handle_sctp_input(Socket, AncData, Data)
  when is_list(Data) ->
  handle_sctp_input(Socket, AncData, list_to_binary(Data));
handle_sctp_input(_Socket, [#sctp_sndrcvinfo{assoc_id = _Assoc}], Data)
  when is_binary(Data) ->
  lager:info("receive: ~p", [binary_to_term(Data)]);
handle_sctp_input(_Socket, _, #sctp_sndrcvinfo{} = Info) ->
  lager:info("socket info: ~p", [lager:pr(Info, ?MODULE)]);
handle_sctp_input(_Socket, _, #sctp_assoc_change{} = Assoc) ->
  lager:info("new socket assoc: ~p", [lager:pr(Assoc, ?MODULE)]);
handle_sctp_input(_Socket, _, #sctp_paddr_change{} = Addr) ->
  lager:info("sctp addr info: ~p", [lager:pr(Addr, ?MODULE)]);
handle_sctp_input(_Socket, _, #sctp_adaptation_event{} = Adapt) ->
  lager:info("adaptation event: ~p", [lager:pr(Adapt, ?MODULE)]);
handle_sctp_input(_Socket, _, #sctp_shutdown_event{assoc_id = Assoc}) ->
  lager:info("closed assotiation: ~p", [Assoc]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
