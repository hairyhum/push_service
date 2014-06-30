-module(push_service).
-behaviour(supervisor).
-include("push_service.hrl").

-export([start/0, start/1, stop/0]).
-export([start_link/0]).
-export([init/1]).
-export([send_to_group/2]).

-type message() :: #message{}.

-export_type([message/0, token/0, app_id/0, os_name/0]).

start() ->
  push_service_app:start().

-spec start([{atom(), term()}]) -> ok.
start(Env) when is_list(Env) ->
  [ application:set_env(push_service, Key, Val) || {Key, Val} <- Env ],
  start().

stop() ->
  application:stop(push_service).

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
  case should_send_to_all(Group) of
    true -> send_to_all(Pool, Tokens, Message);
    false -> send_by_one(Pool, Tokens, Message)
  end.

-spec should_send_to_all(#device_group{}) -> boolean().
should_send_to_all(#device_group{ os_name = ios }) -> false;
should_send_to_all(#device_group{ os_name = android }) -> true;
should_send_to_all(_) -> false.

-spec send_to_all(atom(), [binary()], #message{}) -> ok.
send_to_all(Pool, Tokens, Message) ->
  poolboy:transaction(Pool,
    fun(Worker) ->
      gen_server:call(Worker, {send_many, Tokens, Message})
    end).

-spec send_by_one(atom(), [binary()], #message{}) -> ok.
send_by_one(Pool, Tokens, Message) ->
  [ poolboy:transaction(Pool,
      fun(Worker) ->
        gen_server:call(Worker, {send, Token, Message})
      end)
    || Token <- Tokens ].

-spec get_pool(#device_group{}) -> atom().
get_pool(
  #device_group{
    os_name = OsName,
    app_id = AppId,
    debug = Debug
  }) ->
  PoolName = pool_name(OsName, AppId, Debug),
  case whereis(PoolName) of
    undefined ->
      make_pool(PoolName, OsName, AppId, Debug),
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
  ConfArgs = application:get_env(push_service, pool_args, [{size, 2},{max_overflow, 3}]),
  PoolArgs =  [
    {name, {local, PoolName}},
    {worker_module, worker_module(OsName)}
    ] ++ ConfArgs,
  WorkerArgs = {AppId, Debug},
  Child = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),
  supervisor:start_child(?MODULE, Child).

worker_module(ios) -> apn_pusher_worker;
worker_module(android) -> gcm_pusher_worker;
worker_module('android-nokia') -> nns_pusher_worker;
worker_module(Name) -> error({os_name_not_supported, Name}).
