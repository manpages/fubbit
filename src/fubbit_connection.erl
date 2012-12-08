-module (fubbit_connection).

-behaviour(gen_server).

-include("amqp_client/include/amqp_client.hrl").

%% API
-export([
% wroom
   connect/3 % starts connection and opens channel
  ,disconnect/1  % closes channel, stops connection

% generic wrappers
  ,mq_run/3
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
-spec mq_run(pid(), lists:proplist(), lists:proplist()) -> any().
mq_run(PID, ActionDict, ArgDict) ->
  try % fuck me, why there isn't a better way to accomplish that?
    ActionBin = proplists:get_value(action, ActionDict),
    io:format("running binary_to_existing_atom with ~n~p~n", [<<"#new-", ActionBin/binary, "_ok">>]),
    binary_to_existing_atom(<<"#new-", ActionBin/binary, "_ok">>, utf8),
    io:format("ouch~n"),
    mq_call(PID, ActionDict, ArgDict)
  catch _:_ ->
    io:format("catch~n"),
    mq_cast(PID, ActionDict, ArgDict)
  end.

-spec mq_cast(pid(), lists:proplist(), lists:proplist()) -> ok.
mq_cast(PID, ActionDict, ArgDict) ->
  gen_server:cast(PID, {mq_cast, ActionDict, ArgDict}).

-spec mq_call(pid(), lists:proplist(), lists:proplist()) -> any().
mq_call(PID, ActionDict, ArgDict) ->
  gen_server:call(PID, {mq_call, ActionDict, ArgDict}).

% subscribing to messages from queue to pass messages
subscribe(_PID, _Q) -> ok.

% configuration. hopefully, you won't need to touch that.
set_prefetch_count(_PID, _N) -> ok.

%%%%%%%%%%%%%%%%
%% gen_server %%
%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link(?MODULE, [], []).


init(_A) ->
  {ok, #state{}}.


-spec handle_call(
  {connection, lists:proplist(), from, pid()} |
  {mq_call, lists:proplist(), lists:proplist()},
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

handle_call({mq_call, ActionDict, Args}, _, S) ->
  io:format("calling the rabbit~n"),
  ActBin =  proplists:get_value(action, ActionDict),
  New = binary_to_existing_atom(
    <<"#new-", ActBin/binary>>, 
    utf8
  ),
  Ok = binary_to_existing_atom(
    <<"#new-", ActBin/binary, "_ok">>,
    utf8
  ),
  % todo: case relying on output_arity, has_payload and action
  % skip it by now
  Response = amqp_channel:call(
    S#state.channel,
    fubbit_records:'#fromlist-'(Args, fubbit_records:New())
  ),
  case fubbit_records:'#is_record-'(Ok, Response) of
    true -> 
      io:format("the children always followed him"),
      {reply, ok, S};
    _ ->    
      io:format("he made them laugh, oh yes, he did"),
      {reply, Response, S}
  end;


handle_call(R, _F, S) ->
  {reply, R, S}.


-spec handle_cast(
  disconnect | 
  {mq_cast, lists:proplist(), lists:proplist()},
  #state{}
) -> {stop, shutdown, #state{}} | 
  {noreply, #state{}}.

handle_cast(disconnect, State) ->
  amqp_channel:close(State#state.channel),
  amqp_connection:close(State#state.connection),
  {stop, shutdown, State};

handle_cast({mq_cast, ActionDict, Args}, S) ->
  io:format("casting the spell~n"),
  ActBin =  proplists:get_value(action, ActionDict),
  New = binary_to_existing_atom(
    <<"#new-", ActBin/binary>>, 
    utf8
  ),
  % todo: case relying on output_arity, has_payload and action
  % skip it by now
  amqp_channel:cast(
    S#state.channel,
    fubbit_records:'#fromlist-'(Args, fubbit_records:New())
  ),
  {noreply, S};

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
