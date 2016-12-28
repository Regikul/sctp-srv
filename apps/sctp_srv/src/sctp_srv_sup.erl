%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2016 20:38
%%%-------------------------------------------------------------------
-module(sctp_srv_sup).
-author("regikul").

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  new_child/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec new_child(Port :: pos_integer()) -> supervisor:startchild_ret().
new_child(Port) ->
  supervisor:start_child(?SERVER, [Port]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
    {ok, Sup} ->
      lists:foreach(fun new_child/1, lists:seq(10000, 10005)),
      {'ok', Sup};
    _Else -> _Else
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = transient,
  Shutdown = 2000,
  Type = worker,

  Procs = [
    {'sctp_srv_listener', {'sctp_srv_listener', start_link, []}, Restart, Shutdown, Type, ['sctp_srv_listener']}
  ],

  {ok, {SupFlags, Procs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
