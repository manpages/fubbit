-module (fubbit_connection).

-behaviour(gen_server).

%% API
-export([
% wroom
   connect/2 % starts connection and opens channel
  ,disconnect/0  % closes channel, stops connection

% bare bones
  ,declare_queue/1 % declares queue on current channel
  ,declare_queue/0 % declares queue with generated name
  ,declare_exchange/1 % declares exchange
  ,delete_queue/1 % guess what.
  ,delete_exchange/1 %
  ,bind_queue/3 % creates a routing rule
  ,unbind_queue/3 % destroys a routing rule

% eat 'em some cookies
  ,publish/3 % sends message, requires exchange and routing key
  ,publish/2 % sends message, requires routing key

% useless functions
  ,poll/2 % polls queue, requires no_ack to be set
  ,poll/1 % polls queue

% the main function that does pr0xying
  ,subscribe/1 % subscribes to queue and forwards content

% configuration
  ,set_prefetch_count/1 % size of the prefetch buffer
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
connect(_X, _Y) -> {ok, fine}.
disconnect() -> ok.

% service calls
declare_queue(Name) -> Name.
declare_queue() -> <<"r4nd0m">>.
declare_exchange(Name) -> Name.
delete_queue(_Name) -> ok.
delete_exchange(_Name) -> ok.
bind_queue(_Q, _E, _RK) -> ok.
unbind_queue(_Q, _E, _RK) -> ok.

% sendnig messages
publish(_E, _RK, _Msg) -> ok.
publish(_RK, _Msg) -> ok.

% polling queues
poll(_Q, _NoAck) -> <<"r4nd0m">>.
poll(_Q) -> <<"r4nd0m">>.

% subscribing to messages from queue to pass messages
subscribe(_Q) -> ok.

% configuration. hopefully, you won't need to touch that.
set_prefetch_count(_N) -> ok.

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
