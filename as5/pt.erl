-module(pt).
-import(mr,[start/1, stop/1, job/5]).
-import(read_mxm,[from_file/1,parse_track/1]).
-export([test/0,task1/0,task2/0]).

% Compute the total number of words in all songs together.
task1() ->
  {_,Tracks} = read_mxm:from_file("mxm_dataset_test.txt"),
  N1 = now(),
  {ok, MR}  = mr:start(3),
  {ok, AllWordCounts} = mr:job(MR, 
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
                     AllWordCounts),
  mr:stop(MR),

  N2 = now(),
  Diff = timer:now_diff(N2,N1) / 1000000,
  {Sum,Diff}.

% Compute the average number of different words in a song
% and the average total number of words in a song.
task2() ->
  {_,Tracks} = read_mxm:from_file("mxm_dataset_test.txt"),
  N1 = now(),
  {ok, MR}  = mr:start(3),
  {ok, AllSongs} = mr:job(MR, 
                               fun(Track) -> 
                                 {_,_,WordCounts} = read_mxm:parse_track(Track),
                                 WordCounts 
                               end,
                               fun(WordCounts,Acc) -> [WordCounts] ++ Acc end,
                               [],
                               Tracks),

  {ok, {AvgDiff,AvgTotal,Songs}} = mr:job(MR, 
                                          fun(Song) -> 
                                            {length(Song),sumOfValues(Song),1}
                                          end,
                                          fun({SongDiff,SongTotal,1},{AllDiff,AllTotal,Index}) ->
                                            NewAllDiff  = AllDiff  * (Index/(Index+1)) + SongDiff  * (1/(Index+1)),
                                            NewAllTotal = AllTotal * (Index/(Index+1)) + SongTotal * (1/(Index+1)),
                                            {NewAllDiff,NewAllTotal,Index+1}
                                          end,
                                          {0,0,0},
                                          AllSongs),

  mr:stop(MR),
  N2 = now(),
  Diff = timer:now_diff(N2,N1) / 1000000,
  {{AvgDiff,AvgTotal},Diff}.

sumOfValues(List) ->
  sumOfValues_loop(List,0).

sumOfValues_loop([],Sum) ->
  Sum;
sumOfValues_loop(List,Sum) ->
  [{Key,Value}|Rest] = List,
  sumOfValues_loop(Rest,Value + Sum).

task3() ->



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
  
