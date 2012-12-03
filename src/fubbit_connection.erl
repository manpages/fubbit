-module (fubbit_connection).

-behaviour(gen_server).

-include("amqp_client/include/amqp_client.hrl").

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

-record(state, {
   from :: pid()
  ,connection % types?
  ,channel    % anyone?
}).

%%%%%%%%%%%%
%% Macros %%
%%%%%%%%%%%%


%%%%%%%%%
%% API %%
%%%%%%%%%

% core
-spec connect(pid(), lists:proplist(), pid()) -> ok. %% todo: validate retT
connect(PID, ConnectionDict, Origin) -> 
  gen_server:call(PID, {connection, ConnectionDict, from, Origin}).
-spec disconnect(pid()) -> ok.
disconnect(PID) -> 
  gen_server:cast(PID, disconnect).

% service calls
-spec declare_queue(pid(), lists:proplist()) -> binary().
declare_queue(PID, QDict) -> 
  gen_server:call(PID, {declare_queue, QDict}).
declare_queue(PID) -> 
  gen_server:call(PID, declare_queue).
declare_exchange(PID, EDict) ->
  gen_server:call(PID, {declare_exchange, EDict}).
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
  {ok, #state{}}.

-spec handle_call(
  {connection, lists:proplist(), from, pid()} |
  {
    declare_queue |
    declare_exchange, 
  lists:proplist()} |
  declare_queue,
  pid(), #state{}
) -> 
  {reply, 
  ok | binary(), 
  #state{}}.

handle_call({connection, C, from, PID0}, _, State) ->
  {ok, Con} = case proplists:get_value(type, C) of
    direct -> connect_directly_do(C);
    _      -> connect_do(C)
  end,
  {ok, Chan} = amqp_connection:open_channel(Con),
  {reply, ok, State#state{from=PID0,connection=Con,channel=Chan}};

handle_call({declare_queue, C}, _, State) -> 
  #'queue.declare_ok'{queue=Name} = amqp_channel:call(
    State#state.channel,
    fubbit_records:'#fromlist-'(C, #'queue.declare'{})
  ),
  {reply, Name, State};

handle_call(declare_queue, _, State) ->
  #'queue.declare_ok'{queue=Name} = amqp_channel:call(
    State#state.channel, #'queue.declare'{}),
  {reply, Name, State};

handle_call(R, _F, S) ->
  {reply, R, S}.
 
handle_cast(disconnect, State) ->
  amqp_channel:close(State#state.channel),
  amqp_connection:close(State#state.connection),
  {stop, shutdown, State};
handle_cast(_, S) ->
  {noreply, S}.

%% fukken pr0xy
handle_info(_, S) ->
  {noreply, S}.

code_change(_, S, _) ->
  {ok, S}.

terminate(shutdown, _S) ->
  ok.

%%%%%%%%%%%%%
%% private %%
%%%%%%%%%%%%%

-spec connect_directly_do(lists:proplist()) -> {ok, any()}. %any?
connect_directly_do(C) ->
  amqp_connection:start(
    fubbit_records:'#fromlist-'(C, #amqp_params_direct{})
  ).

-spec connect_do(lists:proplist()) -> {ok, any()}. %any?
connect_do(C) ->
  amqp_connection:start(
    fubbit_records:'#fromlist-'(C, #amqp_params_network{})
  ).
