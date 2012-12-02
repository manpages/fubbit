-module(fubbit_proxy).

-export([
   proxy/3   % starts fubbit_connection and returns its pid
  ,publish/4 % sends (connection_pid, exchange, routekey, msg) 
  ,publish/3 % sends (connection_pid, routekey, msg)
]).

-spec proxy(lists:proplist(), [binary()], pid()) -> pid().
proxy(ConnectionDict, Queries, From) ->
  {ok, PID} = supervisor:start_child(fubbit_sup, []),
  fubbit_connection:connect(PID, ConnectionDict, From),
  lists:each(fun(Q) -> fubbit_connection:subscribe(PID, Q) end, Queries),
  PID.

-spec publish(pid(), binary(), binary(), binary()) -> ok.
publish(PID, Exchange, RouteKey, Message) ->
  fubbit_connection:publish(PID, Exchange, RouteKey, Message).

-spec publish(pid(), binary(), binary()) -> ok.
publish(PID, RouteKey, Message) ->
  fubbit_connection:publish(PID, RouteKey, Message).
