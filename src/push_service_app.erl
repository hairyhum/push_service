-module(push_service_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).


start() ->
  application:start(asn1),
  crypto:start(),
  apns:start(),
  application:start(push_service).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  push_service_sup:start_link().

stop(_State) ->
  ok.
