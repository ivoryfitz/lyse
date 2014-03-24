%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2014 1:19 PM
%%%-------------------------------------------------------------------
-module(dog_fsm).
-author("matt_fitz").

%% API
-compile(export_all).

start() ->
  spawn(fun() -> bark() end).

pet(Pid) -> Pid ! pet.
squirrel(Pid) -> Pid ! squirrel.

bark() ->
  io:format("Dog says: BARK BARK~n"),
  receive
    pet ->
      wag_tail();
    _ ->
      io:format("Dog is confused"),
      bark()
  after 2000 ->
    bark()
  end.

wag_tail() ->
  io:format("Dog wags tail"),
  receive
    pet ->
      sit();
    _ ->
      io:format("Dog is confused"),
      wag_tail()
  after 30000 ->
    bark()
  end.

sit() ->
  io:format("Dog is sitting"),
  receive
    squirrel ->
      bark();
    _ ->
      io:format("Dog is confused"),
      sit()
  after 30000 ->
    bark()
  end.
