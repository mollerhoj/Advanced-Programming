-module(facein_spec).
-import(facein,[start/1,add_friend/2,friends/1,broadcast/3,received_messages/1,shutdown/1,name/1]).
-export([tests/0]).

tests() ->
  test1() and
  test2() and
  test3() and
  test4() and
  test5() and
  test6() and
  test7() and
  test8().

%% A new person has 0 friends
test1() ->
  {_,John} = start("Mr. John"),
  Result = friends(John) =:= [],
  shutdown(John),
  Result.

%% We can ask for and receive the name of a person
test2() ->
  {_,John} = start("Mr. John"),
  Result = name(John) =:= "Mr. John",
  shutdown(John),
  Result.

%% We can ask for and receive the name of a person
test3() ->
  {_,John} = start("Mr. John"),
  {_,Kim} = start("Doc. Kim"),
  Result = add_friend(John,Kim) =:= ok,
  shutdown(John),
  shutdown(Kim),
  Result.

%% A person can make a friend, and return the list of friends.
test4() ->
  {_,John} = start("Mr. John"),
  {_,Kim} = start("Doc. Kim"),
  add_friend(John,Kim),
  Result = friends(John) =:= [{"Doc. Kim",Kim}],
  shutdown(John),
  shutdown(Kim),
  Result.

%% Broadcasting with radius 2 sends to the friends of friends.
test5() ->
  {_,John} = start("Mr. John"),
  {_,Kim} = start("Doc. Kim"),
  {_,Lena} = start("Mrs. Lena"),
  add_friend(Kim,Lena),
  add_friend(John,Kim),
  broadcast(John,"Hi guys!",2),
  timer:sleep(5),
  Result = received_messages(Lena) =:= [{"Mr. John","Hi guys!"}],
  shutdown(John),
  shutdown(Kim),
  shutdown(Lena),
  Result.

%% Does not send message with the same ref twice
test6() ->
  {_,John} = start("Mr. John"),
  {_,Kim} = start("Doc. Kim"),
  {_,Lena} = start("Mrs. Lena"),
  {_,Joe} = start("Joey"),
  {_,Peter} = start("Peter pan"),
  add_friend(John,Kim),
  add_friend(John,Lena),
  add_friend(Kim,Joe),
  add_friend(Lena,Joe),
  add_friend(Joe,Peter),
  broadcast(John,"Hi guys!",3),
  timer:sleep(10),
  Result = received_messages(Peter) =:= [{"Mr. John","Hi guys!"}],
  shutdown(John),
  shutdown(Kim),
  shutdown(Lena),
  shutdown(Joe),
  shutdown(Peter),
  Result.


%% ---------- Answers to questions ------------

%% This is how to build the graph.
graph() ->
  % Create people.
  {_,Ken}     = start("Ken Friis Larsen"),
  {_,Andrzej} = start("Andrzej Filinski"),
  {_,Susan}   = start("Susan Storm"),
  {_,Reed}    = start("Reed Richards"),
  {_,Jessica} = start("Jessica Drew"),
  {_,Tony}    = start("Tony Stark"),
  {_,Jen}     = start("Jen Walters"),
  % Add connections
  add_friend(Ken,Andrzej),
  add_friend(Andrzej,Ken),
  add_friend(Andrzej,Susan),
  add_friend(Susan,Andrzej),
  add_friend(Susan,Reed),
  add_friend(Susan,Jessica),
  add_friend(Susan,Jen),
  add_friend(Reed,Jessica),
  add_friend(Reed,Tony),
  add_friend(Jessica,Jen),
  add_friend(Jen,Susan),
  add_friend(Jen,Jessica),
  add_friend(Jen,Tony),
  % Return graph as a list of people
  [Ken,Andrzej,Susan,Reed,Jessica,Tony,Jen].

%% This is how to kill all the people in the graph
shutdown_graph([]) ->
  ok;
shutdown_graph([Head|Tail]) ->
  shutdown(Head),
  shutdown_graph(Tail).

%% The result of calling `friends` on Jen Walters, is a list of pids,
%% the pids for: [Tony,Jessica,Susan] in no particular order
test7() ->
  Graph = graph(),
  [_,_,Susan,_,Jessica,Tony,Jen] = Graph,
  Result = lists:member({"Tony Stark",Tony},friends(Jen)) and
           lists:member({"Jessica Drew",Jessica},friends(Jen)) and
           lists:member({"Susan Storm",Susan},friends(Jen)),
  shutdown_graph(Graph),
  Result.

%% This is how to broadcast with radius 2 from Jessica Drew
test8() ->
  Graph = graph(),
  [Ken,_,Susan,Reed,Jessica,Tony,_] = Graph,
  broadcast(Jessica,"Hey!",2),
  broadcast(Ken,"Hello!",3),
  timer:sleep(5),
  Result1 = received_messages(Tony) =:= [{"Jessica Drew","Hey!"}],
  Result2 = received_messages(Reed) =:= [{"Ken Friis Larsen","Hello!"}],
  Result3 = lists:member({"Ken Friis Larsen","Hello!"},received_messages(Susan)) and
    lists:member({"Jessica Drew","Hey!"},received_messages(Susan)),
  shutdown_graph(Graph),
  Result1 and Result2 and Result3.
