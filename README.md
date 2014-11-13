push-service
===============

Mobile push services aggregator for Erlang services.

Used to send Push notifications to mobile clients from erlang server.

Supports iOS APN, Android GCM and Nokia push backends. 

Easy to add new backends by implementing `pusher_worker` modules.

Using process pool to start `pusher_workers` with [Poolboy](https://github.com/devinus/poolboy)

Each pusher process is defined by `os_name` and `app_id`

### Usage

Add rebar dependency:

```erlang
{push_service, ".*", {git, "https://github.com/hairyhum/push_service.git"}},
```

Add to *.app.src file:

```erlang
{applications, [
                push_service,
                ...
               ]}
```

Or start manualy:

```erlang
push_service:start()
```

Then you can send push notifications for some devices:

```erlang
-include_lib("push_service/include/push_service.hrl").

...

push_service:send_to_group(Group, Message)

```

Where `Group::#device_group{}` and `Message::#message{}` is defined in `push_service.hrl`

```erlang
-type token() :: binary() | string().
-type app_id() :: binary() | string().
-type os_name() :: atom().

-record(device_group, {
  tokens :: list(token()), % List of device tokens (device ids) to send to
  os_name :: os_name(),    % Atom os name (currently supported ios, android and android-nokia).
  app_id :: app_id(),      % App identifier. In IOS selects sertificate, in Android selects ApiKey
  debug :: boolean()       % Debug flag. Used in ios to select push server
  }).

-record (message, {
  text  = none :: binary(),          % Main message text
  badge = none :: integer(),         % Badge for ios push
  sound = none :: binary(),          % Sound for ios push
  extra = []   :: [{atom(), term()}] % Other info, tat can be added in message
  }).
```

Devices should be grouped by `os_name` and `app_id`. 

### Configuration

Application is configured by `app_env`

Configuration example (from sys.config)

```erlang
{push_service, [
  {workers, [                     % Worker modules
    {ios, apn_pusher_worker},
    {android, gcm_pusher_worker},
    {'android-nokia', nns_pusher_worker}
  ]},
  {pool_args, [                   % Poolboy pool arguments.
    {size, 10},
    {max_overflow, 20}
  ]},
  {apn, [  % Apn worker config
    {cert_dir, "etc/config/apn"}, % Directory where to look for certificates 
    {feedback_fun, {mod, fun}},   % APN Feedback callback fun(app_id(), {calendar:datetime(), string()}) -> _ (optional, default defined in apn_pusher_worker)
    {error_fun, {mod, fun}},      % Error callback fun((binary(), apns:status()) -> stop | _ (optional, default defined in apn_pusher_worker)
    {timeout, 600000},            % Send timeout (optional, default = 600000)
    {no_keyfile, false},          % True if sender should be started without certificate (optioal, default = false)
    {cert_password, "foo"}        % Sertificate password (optional, default = undefined)
  ]},
  {gcm, [
    {api_keys, [                  % API keys for each app_id
         {"com.app.one", "123"},
         {"com.app.two", "456"},
    ]},
    {error_fun, {mod, fun}}       % Error callback fun(binary(), token() | {token(), token()}) -> _
  ]},
  {nns, []}                         % Same as for GCM
]},
```

Certificates for APN should be in `cert_dir` under following structure:

```
  apn/
    <app_id1>/
      apn_production.pem  # For debug = false
      apn_development.pem # For debug = true
    <app_id2>/
      apn_production.pem
      apn_development.pem
```


### Worker modules

You can define your own `pusher_worker`. 

It should implement `gen_server`, `poolboy_worker` and `pusher_worker` behaviours and should be listed in `workers` in config.

Worker module should handle `gen_server:call` events `{send, token(), #message{}}` and `{send_many, [token()], #message{}}`

