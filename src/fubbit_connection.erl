-module (fubbit_connection).

-behaviour(gen_server).

%% API
-export([
% wroom
   connect/3 % starts connection and opens channel
  ,disconnect/1  % closes channel, stops connection

% bare bones
  ,declare_queue/2 % declares queue on current channel
  ,declare_queue/1 % declares queue with generated name
  ,declare_exchange/2 % declares exchange
  ,delete_queue/2 % guess what.
  ,delete_exchange/2 %
  ,bind_queue/4 % creates a routing rule
  ,unbind_queue/4 % destroys a routing rule

% eat 'em some cookies
  ,publish/4 % sends message, requires exchange and routing key
  ,publish/3 % sends message, requires routing key

% useless functions
  ,poll/3 % polls queue, requires no_ack to be set
  ,poll/2 % polls queue

% the main function that does pr0xying
  ,subscribe/2 % subscribes to queue and forwards content

% configuration
  ,set_prefetch_count/2 % size of the prefetch buffer
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

%%%%%%%%%%%
%% State %%
%%%%%%%%%%%

%%%%%%%%%%%%
%% Macros %%
%%%%%%%%%%%%

%%%%%%%%%
%% API %%
%%%%%%%%%

% core
connect(PID, _X, _Y) -> {ok, fine}.
disconnect(PID) -> ok.

% service calls
declare_queue(PID, Name) -> Name.
declare_queue(PID) -> <<"r4nd0m">>.
declare_exchange(PID, Name) -> Name.
delete_queue(PID, _Name) -> ok.
delete_exchange(PID, _Name) -> ok.
bind_queue(PID, _Q, _E, _RK) -> ok.
unbind_queue(PID, _Q, _E, _RK) -> ok.

% sendnig messages
publish(PID, _E, _RK, _Msg) -> ok.
publish(PID, _RK, _Msg) -> ok.

% polling queues
poll(PID, _Q, _NoAck) -> <<"r4nd0m">>.
poll(PID, _Q) -> <<"r4nd0m">>.

% subscribing to messages from queue to pass messages
subscribe(PID, _Q) -> ok.

% configuration. hopefully, you won't need to touch that.
set_prefetch_count(PID, _N) -> ok.

%%%%%%%%%%%%%%%%
%% gen_server %%
%%%%%%%%%%%%%%%%

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
