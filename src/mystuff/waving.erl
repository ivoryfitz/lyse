%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 12. Mar 2014 9:14 AM
%%%-------------------------------------------------------------------
-module(waving).
-author("matt_fitz").

%% API
-compile(export_all).


startMatt() ->
  spawn(fun() -> mattIdle() end).

mattIdle() ->
  io:format("Matt is Idling~n"),
  Ref = make_ref(),
  receive
    {waveTo, Pid} ->
      Pid ! {waveFrom, self(), Ref},
      mattWaving(Ref);
    {waveFrom, Pid, ARef} ->
      Pid ! {waveBack, self(), ARef},
      mattIdle()
  after 3000 ->
    io:format("Matt kicks at the dirt...~n"),
    mattIdle()
  end.

mattWaving(Ref) ->
  io:format("Matt is Waving!~n"),
  receive
    {waveBack, Pid, Ref} ->
      io:format("~p waved back! Yay!~n", [Pid]),
      mattIdle()
  end.


