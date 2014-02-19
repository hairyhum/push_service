-module(gcm_pusher_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("push_service.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([error_fun/2]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({AppId, Debug}) when is_binary(AppId) ->
  init({binary_to_list(AppId), Debug});
init({AppId, Debug}) when is_list(AppId) ->
  Config = application:get_env(push_service, gcm, []),
  ApiKeys = proplists:get_value(api_keys, Config, []),
  ErrorFun = case proplists:get_value(error_fun, Config) of
    {ErrorFunMod, ErrorFunFun} -> fun ErrorFunMod:ErrorFunFun/2;
    undefined -> undefined
  end,
  case proplists:get_value(AppId, ApiKeys) of
    undefined ->
      {stop, no_config_for_app_id};
    ApiKey ->
      Name = list_to_atom("gcm_conn_" ++ AppId ++ if Debug -> "debug"; true -> "" end),
      {ok, Conn} = case ErrorFun of
        undefined -> gcm:start_link(Name, ApiKey);
        F when is_function(F, 2)  -> gcm:start_link(Name, ApiKey, F)
      end,
      {ok, #state{ conn = Conn }}
  end.

get_message(#message{
  text = Text,
  extra = Extra
  }) ->
  BinExtra = [ { atom_to_binary(Key, utf8), Val } || {Key, Val} <- Extra ],
  [{<<"data">>, [{<<"message">>, Text}] ++ BinExtra}].

get_tokens(Tokens) -> Tokens.

error_fun(X, Y) ->
  erlang:display({X,Y}).

handle_call({send, Tokens, Message}, _From, #state{conn = C} = State)
    when is_record(Message, message) ->
  ok = gcm:sync_push(C, get_tokens(Tokens), get_message(Message)),
  {reply, ok, State}.

handle_cast(_,State) -> {noreply, State}.

handle_info(_,State) -> {noreply, State}.

terminate(_, #state{ conn = C }) -> gcm:stop(C).

code_change(_,State,_) -> {ok, State}.
