-module(pt).
-import(mr,[start/1, stop/1, job/5]).
-import(read_mxm,[from_file/1,parse_track/1]).
-export([test/0,task1/0,task2/0]).

task1() ->
  {_,Tracks} = read_mxm:from_file("mxm_dataset_test.txt"),
  N1 = now(),
  {ok, MR}  = mr:start(3),
  {ok, AWC} = mr:job(MR, 
                     fun(Track) -> 
                       {_,_,WordCounts} = read_mxm:parse_track(Track),
                       WordCounts 
                     end,
                     fun(WordCounts,Acc) -> WordCounts ++ Acc end,
                     [],
                     Tracks),

  {ok, Sum} = mr:job(MR, 
                     fun({_,Count}) -> Count end,
                     fun(Count,Acc) -> Count + Acc end,
                     0,
                     AWC),
  mr:stop(MR),

  N2 = now(),
  Diff = timer:now_diff(N2,N1) / 1000000,
  {Sum,Diff}.

task2() ->
  ok.

test() ->
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
  
