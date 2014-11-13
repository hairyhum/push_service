-module(pusher_worker).

-callback should_send_to_all() -> boolean().