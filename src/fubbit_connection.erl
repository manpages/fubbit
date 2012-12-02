-module (fubbit_connection).

-behaviour(gen_server).

%% API
-export([
   connect/2 % starts connection and opens channel
  ,disconnect/0  % closes channel, stops connection
]).

%% gen_server
-export([
   start_link/0
  ,init/1
  ,code_change/3
  ,handle_call/3
  ,handle_cast/2
  ,handle_info/2
  ,terminate/2
]).

%% State

%% API
connect(_X, _Y) -> {ok, fine}.
disconnect() -> ok.

%%gen_server
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_A) ->
  {ok, []}.

handle_call(R, _F, S) ->
  {reply, R, S}.
 
handle_cast(_, S) ->
  {noreply, S}.

%% fukken pr0xy
handle_info(_, S) ->
  {noreply, S}.

code_change(_, S, _) ->
  {ok, S}.

terminate(shutdown, _S) ->
  ok.
