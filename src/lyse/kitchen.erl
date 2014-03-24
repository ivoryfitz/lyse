%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2014 2:12 PM
%%%-------------------------------------------------------------------
-module(kitchen).
-author("matt_fitz").

%% API
-compile(export_all).

fridge1(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge1([Food | FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge1(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge1(FoodList)
      end;
    {From, look} ->
      From ! {self(), FoodList},
      fridge1(FoodList);
    terminate ->
      ok
  end.


store(Pid, Food) ->
  io:format("Pid in Store ~p~n", [self()]),
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

look(Pid) ->
  Pid ! {self(), look},
  receive
    {Pid, Msg} -> Msg
  end.

start(FoodList) ->
  spawn(?MODULE, fridge1, [FoodList]).


mainTest() ->
  io:format("Pid in test: ~p~n", [self()]),
  Kitchen = spawn(kitchen, fridge1, [[baking_soda]]),
  TestPid = spawn(fun() -> Msg = kitchen:store(Kitchen, ketchup), io:format("~p~n", [Msg]) end).
