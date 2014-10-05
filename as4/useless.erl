-module(useless).
-export([add/2,hello/0,greet_and_add_two/1]).

add(A,B) ->
  A + B.

hello() ->
  io:format("Hello world!~n").
 
greet_and_add_two(X) ->
  hello(),
  add(X,2).

%% Pattern matching
greet(male, Name) ->
  io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
  io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
  io:format("Hello, ~s!", [Name]).

%% Guards
wrong_age(X) when X < 16; X > 104 ->
  true;
wrong_age(_) ->
  false.


%% Concurrency


new_f() ->
  F = fun() -> 2 + 2 end,
  spawn(F).
 
calculateArea() ->
  receive
    {rectangle,W, H} ->
      W * H;
    {circle, R} ->
      3.14 * R * R;
    _ ->
    io:format("We failed!")
  end.

dolphin1() ->
  receive
    {From, do_a_flip} ->
      From ! "how about no?",
      dolphin1();
    {From, fish} ->
      From ! "thanks man Cya!";
    _ ->
      io:format("Hah, we're smart!"),
      dolphin1()
  end.

fridge2(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge2([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
      true ->
        From ! {self(), {ok, Food}},
        fridge2(lists:delete(Food, FoodList));
      false ->
        From ! {self(), not_found},
        fridge2(FoodList)
      end;
    terminate ->
      ok
  end.
