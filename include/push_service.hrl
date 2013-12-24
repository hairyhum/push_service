-type token() :: binary().
-type app_id() :: binary().
-type os_name() :: ios | android | 'android-kindle'.


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

