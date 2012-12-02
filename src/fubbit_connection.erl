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
  gen_server:call(PID, disconnect).

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
  {ok, #state{}}.

handle_call({connection, C, from, PID0}, _, State) ->
  {ok, Con} = case proplists:get_value(type, C) of
    direct -> connect_directly_do(C);
    _      -> connect_do(C)
  end,
  {ok, Chan} = amqp_connection:open_channel(Con),
  {reply, ok, State#state{from=PID0,connection=Con,channel=Chan}};
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

%%%%%%%%%%%%%
%% private %%
%%%%%%%%%%%%%

-spec connect_directly_do(lists:proplist()) -> {ok, any()}. %any?
connect_directly_do(C) ->
  P0 = #amqp_params_direct{},
  P1 = P0#amqp_params_direct{
    username=proplists:get_value(username, C, P0#amqp_params_direct.username),
    password=proplists:get_value(password, C, P0#amqp_params_direct.password),
    virtual_host=proplists:get_value(virtual_host, C, P0#amqp_params_direct.virtual_host),
    node=proplists:get_value(node, C, P0#amqp_params_direct.node),
    client_properties=proplists:get_value(client_properties, C, P0#amqp_params_direct.client_properties)
  },
  amqp_connection:start(P1).

-spec connect_do(lists:proplist()) -> {ok, any()}. %any?
connect_do(C) ->
  P0 = #amqp_params_network{},
  P1 = P0#amqp_params_network{
    username=proplists:get_value(username, C, P0#amqp_params_network.username),
    password=proplists:get_value(password, C, P0#amqp_params_network.password),
    virtual_host=proplists:get_value(virtual_host, C, P0#amqp_params_network.virtual_host),
    host=proplists:get_value(host, C, P0#amqp_params_network.host),
    port=proplists:get_value(port, C, P0#amqp_params_network.port),
    channel_max=proplists:get_value(channel_max, C, P0#amqp_params_network.channel_max),
    frame_max=proplists:get_value(frame_max, C, P0#amqp_params_network.frame_max),
    heartbeat=proplists:get_value(heartbeat, C, P0#amqp_params_network.heartbeat),
    ssl_options=proplists:get_value(ssl_options, C, P0#amqp_params_network.ssl_options),
    auth_mechanisms=proplists:get_value(auth_mechanisms, C, P0#amqp_params_network.auth_mechanisms),
    client_properties=proplists:get_value(client_properties, C, P0#amqp_params_network.client_properties)
  },
  amqp_connection:start(P1).
