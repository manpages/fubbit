-module(fubbit_app).

-behaviour(application).

%% 
-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

start() ->
  application:start(fubbit_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  % we start supervisor for illustration.
  % in the real applications please plug fubbit_connection
  % in a sofo/pool -- one per connection to rabbitmq server.
  fubbit_sup:start_link().

stop(_State) ->
  ok.
