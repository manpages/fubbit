-module (fubbit_connection).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([
% wroom
   connect/3 % starts connection and opens channel
  ,disconnect/1  % closes channel, stops connection

% generic wrappers
  ,mq_run/3
  ,mq_run/4

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
-spec mq_run(pid(), lists:proplist(), lists:proplist(), lists:proplist()) -> any().
mq_run(PID, ActionDict, ArgDict, PayloadDict) ->
  try 
    ActionBin = proplists:get_value(action, ActionDict),
    io:format("running binary_to_existing_atom with ~n~p~n", [<<"#new-", ActionBin/binary, "_ok">>]),
    binary_to_existing_atom(<<"#new-", ActionBin/binary, "_ok">>, utf8),
    io:format("ouch~n"),
    mq_call(PID, ActionDict, ArgDict, PayloadDict)
  catch _:_ ->
    io:format("catch~n"),
    mq_cast(PID, ActionDict, ArgDict, PayloadDict)
  end.

-spec mq_run(pid(), lists:proplist(), lists:proplist()) -> any().
mq_run(PID, ActionDict, ArgDict) ->
  try % fuck me, why there isn't a better way to accomplish that?
    ActionBin = proplists:get_value(action, ActionDict),
    io:format("running binary_to_existing_atom with ~n~p~n", [<<"#new-", ActionBin/binary, "_ok">>]),
    binary_to_existing_atom(<<"#new-", ActionBin/binary, "_ok">>, utf8),
    io:format("ouch~n"),
    mq_call(PID, ActionDict, ArgDict, [])
  catch _:_ ->
    io:format("catch~n"),
    mq_cast(PID, ActionDict, ArgDict, [])
  end.

-spec mq_cast(pid(), lists:proplist(), lists:proplist(), lists:proplist()) -> ok.
mq_cast(PID, ActionDict, ArgDict, PayloadDict) ->
  gen_server:cast(PID, {mq_cast, ActionDict, ArgDict, PayloadDict}).

-spec mq_call(pid(), lists:proplist(), lists:proplist(), lists:proplist()) -> any().
mq_call(PID, ActionDict, ArgDict, PayloadDict) ->
  gen_server:call(PID, {mq_call, ActionDict, ArgDict, PayloadDict}).

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
  {mq_call, lists:proplist(), lists:proplist(), lists:proplist()},
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

handle_call({mq_call, ActionDict, Args, PayloadArgs}, _, S) ->
  io:format("calling the rabbit~n"),
  ActionBin =  proplists:get_value(action, ActionDict),
  PayloadBin = proplists:get_value(payload, ActionDict),
  NewActionF = binary_to_existing_atom(
    <<"#new-", ActionBin/binary>>, 
    utf8
  ),
  NewPayloadF = case PayloadBin of
    undefined -> undefined; 
    _ -> binary_to_existing_atom(
      <<"#new-", PayloadBin/binary>>,
      utf8
    )
  end,
  OkT = binary_to_existing_atom(
    <<ActionBin/binary, "_ok">>,
    utf8
  ),

  Response = case NewPayloadF of
    undefined -> amqp_channel:call(
      S#state.channel,
      fubbit_records:'#fromlist-'(Args, fubbit_records:NewActionF())
    );
    _ -> amqp_channel:call(
      S#state.channel,
      fubbit_records:'#fromlist-'(Args, fubbit_records:NewActionF()),
      fubbit_records:'#fromlist-'(PayloadArgs, fubbit_records:NewPayloadF())
    )
  end,

  case Response of
    {OkRec, Content} -> case fubbit_records:'#is_record-'(OkT, OkRec) of
      true ->
        io:format("you've never gone too far to let go, all that you believe in.~n"),
        {reply, {ok, { 
          fubbit_records:to_list(OkT, OkRec), 
          Content
        }}, S};
      false -> % sadly, this far consumer of _connection has to handle notoks manually
        io:format("you've never gone too far to turn back.~n"),
        {reply, {notok, {OkRec, Content}}, S}
      end;
    _ -> case fubbit_records:'#is_record-'(OkT, Response) of
      true -> 
        io:format("the children always followed him~n"),
        {reply, {ok, fubbit_records:to_list(OkT, Response)}, S};
      _ ->    
        io:format("he made them laugh, oh yes, he did~n"),
        {reply, {notok, Response}, S}
    end
  end;


handle_call(R, _F, S) ->
  {reply, R, S}.


-spec handle_cast(
  disconnect | 
  {mq_cast, lists:proplist(), lists:proplist(), lists:proplist()},
  #state{}
) -> {stop, shutdown, #state{}} | 
  {noreply, #state{}}.

handle_cast(disconnect, State) ->
  amqp_channel:close(State#state.channel),
  amqp_connection:close(State#state.connection),
  {stop, shutdown, State};

handle_cast({mq_cast, ActionDict, Args, PayloadArgs}, S) ->
  io:format("casting the spell~n"),
  ActionBin =  proplists:get_value(action, ActionDict),
  PayloadBin = proplists:get_value(payload, ActionDict),
  NewActionF = binary_to_existing_atom(
    <<"#new-", ActionBin/binary>>, 
    utf8
  ),
  NewPayloadF = case PayloadBin of
    undefined -> undefined; 
    _ -> binary_to_existing_atom(
      <<"#new-", PayloadBin/binary>>,
      utf8
    )
  end,

  case NewPayloadF of
    undefined -> amqp_channel:cast(
      S#state.channel,
      fubbit_records:'#fromlist-'(Args, fubbit_records:NewActionF())
    );
    _ -> amqp_channel:cast(
      S#state.channel,
      fubbit_records:'#fromlist-'(Args, fubbit_records:NewActionF()),
      fubbit_records:'#fromlist-'(PayloadArgs, fubbit_records:NewPayloadF())
    )
  end,

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
