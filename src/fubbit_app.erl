-module(fubbit_app).

-behaviour(application).

%% 
-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

start() ->
  fubbit_records:module_info(), % really important shit. do not remove
  application:start(fubbit).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  fubbit_sup:start_link().

stop(_State) ->
  ok.
