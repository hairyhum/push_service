-module(pusher).
-behaviour(supervisor).
-include("push_service.hrl").

-export([start_link/0]).
-export([init/1]).
-export([send_to_group/2]).

-type pusher() :: pid().
-type message() :: #message{}.

-export_type([message/0, token/0, app_id/0, os_name/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

-spec send_to_group(#device_group{}, #message{}) -> ok.
send_to_group(
  #device_group{
    tokens = Tokens
  } = Group,
  Message) ->
  Pool = get_pool(Group),
  poolboy:transaction(Pool,
    fun(Pusher) ->
      send(Pusher, Tokens, Message)
    end).

-spec get_pool(#device_group{}) -> pusher().
get_pool(
  #device_group{
    os_name = OsName,
    app_id = AppId,
    debug = Debug
  }) ->
  PoolName = pool_name(OsName, AppId, Debug),
  case whereis(PoolName) of
    undefined ->
      {ok, _} = make_pool(PoolName, OsName, AppId, Debug),
      PoolName;
    Pid when is_pid(Pid) ->
      PoolName
  end.

-spec pool_name(os_name(), app_id(), boolean()) -> atom().
pool_name(OsName, AppId, Debug) ->
  list_to_atom(
    "pusher_" ++
    atom_to_list(OsName) ++
    binary_to_list(AppId) ++
    if Debug -> "debug"; true -> "production" end).

-spec make_pool(atom(), os_name(), app_id(), boolean()) -> {ok, pid()}.
make_pool(PoolName, OsName, AppId, Debug) ->
  {ok, PoolArgs} = application:get_env(push_service, pool_args),
  PoolArgs =  [
    {name, {local, PoolName}},
    {worker_module, worker_module(OsName)}
    ] ++ PoolArgs,
  WorkerArgs = [AppId, Debug],
  Child = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),
  supervisor:start_child(?MODULE, Child).

-spec send(pid(), [token()], message()) -> ok.
send(Pusher, Tokens, Message) ->
  gen_server:call(Pusher, {send, Tokens, Message}).

worker_module(ios) -> apn_pusher_worker;
worker_module(android) -> gcm_pusher_worker;
worker_module('android-kindle') -> adm_pusher_worker.
