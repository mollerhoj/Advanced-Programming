-module(mr).
-compile(mr).
-export([start/1, stop/1, job/5]).

%%%% Interface
private_test() ->
    {ok, MR}  = mr:start(3),
    {ok, Sum} = mr:job(MR, 
                       fun(X) -> X end,
                       fun(X,Acc) -> X+Acc end,
                       0,
                       lists:seq(1,10)),
    {ok, Fac} = mr:job(MR, 
                       fun(X) -> X end,
                       fun(X,Acc) -> X*Acc end,
                       1,
                       lists:seq(1,10)),
    mr:stop(MR),
    {Sum, Fac}.

start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.

stop(Pid) ->
  rpc(Pid,stop).

job(CPid, MapFun, RedFun, RedInit, Data) ->
  rpc(CPid,{init, MapFun, RedFun, RedInit, Data}),
  rpc(CPid,{run, Data}).

%%%% Internal implementation

init(N) ->
  Reducer = spawn(fun() -> reducer_loop(no_cpid,no_acc,no_fun,0) end),
  Mappers = [spawn(fun() -> mapper_loop(Reducer,no_fun) end) || _ <- lists:seq(1,N)],
  
  {Reducer,Mappers}.

%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

%% asynchronous communication

async(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    async(Pid, stop).

data_async(Pid, D) ->
    async(Pid, {data, D}).

reduce_async(Pid,Result) ->
    async(Pid, {reduce,Result}).

%%% Coordinator
coordinator_loop(Reducer, Mappers) ->
    receive
	{From, stop} ->
	    %io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    reply_ok(From);
	{From, {init, MapFun, RedFun, RedInit, Data}} ->
	    %io:format("~p initializing~n", [self()]),
      Remaining = length(Data),
      [async(Mapper,{init,MapFun}) || Mapper <- Mappers],
      async(Reducer,{init,self(),RedInit,RedFun,Remaining}),
      reply_ok(From),
      coordinator_loop(Reducer, Mappers); % Maybe sync?
	{From, {run, Data}} ->
	    %io:format("~p running~n", [self()]),
      send_data(Mappers, Data),
      receive
        {result,Result} ->
	        %io:format("~p got some!~n", [self()])
          reply_ok(From, Result),
          coordinator_loop(Reducer, Mappers) % Maybe sync?
      end
    end.

send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).

callback(CPid,Result,0) ->
  CPid ! {result,Result};
callback(_,_,_) ->
  ok.

%%% Reducer
reducer_loop(CPid,Acc, RedFun,Remaining) ->
    receive
	stop -> 
	    %io:format("Reducer ~p stopping~n", [self()]),
	    ok;
  {init,InitCPid,InitRedInit,InitRedFun,InitRemaining} ->
	    %io:format("Reducer ~p initializing~n", [self()]),
      reducer_loop(InitCPid,InitRedInit,InitRedFun,InitRemaining);
  {reduce,X} ->
	    %io:format("Reducer ~p reducing~n", [self()]),
      NewAcc = RedFun(X,Acc),
      callback(CPid,NewAcc,Remaining-1),
      reducer_loop(CPid,NewAcc,RedFun,Remaining-1)
    end.

% gather_data_from_mappers(Fun, Acc, Missing) ->
%     receive
%   _ ->
%     not_ok
%     end.

%%% Mapper
mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    %io:format("Mapper ~p stopping~n", [self()]),
	    ok;
  {init,NewFun} ->
	    %io:format("Mapper ~p initializing~n", [self()]),
      mapper_loop(Reducer,NewFun);
  {data, D} ->
	    %io:format("Mapper ~p running~n", [self()]),
      Result = Fun(D),
      reduce_async(Reducer,Result),
	    mapper_loop(Reducer, Fun);
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.
