-type token() :: binary() | string().
-type app_id() :: binary() | string().
-type os_name() :: ios | android.


-record(device_group, {
  tokens :: list(token()),
  os_name :: os_name(),
  app_id :: app_id(),
  debug :: boolean()
  }).

-record (message, {
  text  = none :: binary(),
  badge = none :: integer(),
  sound = none :: binary(),
  extra = []   :: [{atom(), term()}]
  }).

