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
  fubbit_sup:start_link().

stop(_State) ->
  ok.
