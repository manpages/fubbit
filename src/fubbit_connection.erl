-module (fubbit_connection).

-behaviour(gen_server).

-include("amqp_client/include/amqp_client.hrl").

%% API
-export([
% wroom
   connect/3 % starts connection and opens channel
  ,disconnect/1  % closes channel, stops connection

% generic wrappers
  ,mq_cast/3 % asynchronous request to amqp_client
  ,mq_call/3 % synchronous call to amqp_client

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

% wrappers
-spec mq_cast(pid(), lists:proplist(), lists:proplist()) -> ok.
mq_cast(PID, RecSpecDict, ArgDict) ->
	ok.

-spec mq_call(pid(), lists:proplist(), lists:proplist()) -> lists:proplist().
mq_call(PID, RecSpecDict, ArgDict) ->
	ok.


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
