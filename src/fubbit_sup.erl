
-module(fubbit_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(IMMORTAL(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(TRANSIENT(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).
-define(MORTAL(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [
    ?TRANSIENT(fubbit_connection, worker)
  ]} }.
