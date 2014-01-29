-module(apn_pusher_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("push_service.hrl").
-include_lib("apns/include/apns.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([error_fun/2, feedback_fun/1]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

error_fun(X, Y) ->
  erlang:display({X,Y}).

feedback_fun({Date, Token}) ->
  erlang:display({Date, Token}).

init({AppId, Debug}) ->
  Config = application:get_env(push_service, apn, []),

  {ErrorFunMod, ErrorFunFun} = proplists:get_value(error_fun, Config, {apn_pusher_worker, error_fun}),
  ErrorFun = fun ErrorFunMod:ErrorFunFun/2,
  {FeedbackFunMod, FeedbackFunFun} = proplists:get_value(feedback_fun, Config, {apn_pusher_worker, feedback_fun}),
  FeedbackFun = fun FeedbackFunMod:FeedbackFunFun/1,

  Timeout = proplists:get_value(timeout, Config, 30000),
  FeedbackTimeout = proplists:get_value(feedback_timeout, Config, 30*60*1000),

  CertDir = proplists:get_value(cert_dir, Config, "etc/apn"),

  CertName = case Debug of
    true -> "apn_development.pem";
    false -> "apn_production.pem"
  end,
  CertFile = CertDir ++ "/" ++ binary_to_list(AppId) ++ "/" ++ CertName,

  NoKeyFile = proplists:get_value(no_keyfile, Config, false),

  KeyFile = case NoKeyFile of
    true -> undefined;
    false ->
      KeyName = case Debug of
        true -> "apn_development_key.pem";
        false -> "apn_production_key.pem"
      end,
      CertDir ++ "/" ++ binary_to_list(AppId) ++ "/" ++ KeyName
  end,

  Passphrase = proplists:get_value(cert_password, Config),
  case file:read_file_info(CertFile)  of
    {ok, _} ->
      Connection = #apns_connection{
        cert_file = CertFile,
        key_file = KeyFile,
        cert_password = Passphrase,
        apple_host = push_host(Debug),
        feedback_host = feedback_host(Debug),
        feedback_fun = FeedbackFun,
        error_fun = ErrorFun,
        timeout = Timeout,
        feedback_timeout = FeedbackTimeout
      },
      {ok, Pid} = apns:connect(Connection),
      {ok, #state{ conn = Pid }};
    {error, Reason} ->
      {stop, {cert_file, Reason}}
  end.

handle_call({send, Tokens, Message}, _From, #state{conn = C} = State)
    when is_record(Message, message) ->
  [ {ok, _} = apns:send_sync_message(C, get_message(Message, Token))
    || Token <- Tokens ],
  {reply, ok, State}.

handle_cast(_,State) -> {noreply, State}.

handle_info(_,State) -> {noreply, State}.

terminate(_, #state{ conn = C }) -> apns:disconnect(C).

code_change(_,State,_) -> {ok, State}.

-spec get_message(push_service:message(), push_service:token()) -> #apns_msg{}.
get_message(Message, Token) when is_binary(Token) ->
  get_message(Message, binary_to_list(Token));
get_message(
  #message{
    text = Text,
    badge = Badge,
    sound = Sound,
    extra = Extra
  },
  Token) when is_list(Token) ->
  #apns_msg{
    device_token = Token,
    alert = case Text of undefined -> default(text); _ -> Text end,
    badge = case Badge of undefined -> default(badge); _ -> Badge end,
    sound = case Sound of undefined -> default(sound); _ -> Sound end,
    extra = case Extra of undefined -> default(extra); _ -> Extra end
  }.

default(extra) -> [];
default(_) -> none.

push_host(true) -> "gateway.sandbox.push.apple.com";
push_host(false) -> "gateway.push.apple.com".

feedback_host(true) -> "feedback.sandbox.push.apple.com";
feedback_host(false) -> "feedback.push.apple.com".